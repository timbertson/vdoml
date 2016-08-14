open Log_
open Vdom_
open Attr_

let identity x = x

module Ui = struct
  type identity = Vdom.user_tag

  let init_logging () =
    (* setup console *)
    if Logs.reporter () == Logs.nop_reporter then (
      let console = Logs_browser.console_reporter () in
      let reporter = { Logs.report = (fun src level ~over k user_msgf ->
        console.Logs.report src level ~over k (fun console_msgf ->
          user_msgf (fun ?header ?tags fmt ->
            console_msgf ?header ?tags ("%s: @[" ^^ fmt ^^ "@]")
              (Logs.Src.name src)
          )
        )
      )} in
      Logs.set_reporter reporter
    )

  let set_log_level lvl =
    init_logging ();
    Logs.Src.set_level log_src (Some lvl);
    Log.info (fun m -> m "Set log level to %s" (Logs.level_to_string (Some lvl)))

  let identify id = let open Vdom in identify (User_tag id)

  type node = Html_.vdom_node

  type ('elt, 'a) instance_state = {
    state_val: 'a;
    state_counter: int;
    state_view: Vdom.node;
  }

  type 'message emit_fn = 'message -> unit

  and ('elt, 'model, 'message) view_fn = ('elt, 'model, 'message) instance -> 'model -> 'elt Html.elt

  and ('elt, 'model, 'message) component = {
    init: 'model;
    update: 'model -> 'message -> 'model;
    component_view: ('elt, 'model, 'message) view_fn;
  }

  and ('elt, 'model, 'message) instance = {
    emit: 'message emit_fn;
    identity: Vdom.identity;
    instance_view: ('elt, 'model, 'message) view_fn;
    (* This is a bit lame - because we can't encode state in the vdom,
     * we store it (along with the vdom) here. Re-rendering on unchanged
     * state is only skipped if each instance is used only once in the DOM.
     * This is recommended, but can't be enforced. *)
    state: ('elt, 'model) instance_state option ref;
  }

  module State = struct
    let update view_fn current new_state =
      if current.state_val = new_state
        then current
        else {
          state_val = new_state;
          state_counter = current.state_counter + 1;
          state_view = view_fn new_state;
        }

    let init dom state = { state_val = state; state_counter = 0; state_view = dom }
  end

  let gen_identity = let tag_counter = ref 0 in
    let open Vdom in
    function
      | Some id -> User_tag id
      | None ->
        let tag = !tag_counter in
        tag_counter := tag + 1;
        Internal_tag tag

  let wrap_message instance wrapper =
    (fun msg -> instance.emit (wrapper msg))

  let component
      ~(update:'model -> 'message -> 'model)
      ~(view: ('elt, 'model, 'message) view_fn)
      (init:'model) = 
    { init; update; component_view = view }

  let update_and_view instance =
      let id = instance.identity in
      let render = let view = instance.instance_view instance in
        fun state -> Vdom.identify_anonymous id (view state) in
    fun state ->
      (* Note: this is effectful because we need to store `state` separate from VDOM :( *)
      let state = match !(instance.state) with
        | None -> State.init (render state) state
        | Some existing -> State.update render existing state
      in
      instance.state := Some (state);
      state.state_view

  let instantiate component =
    let events, emit = Lwt_stream.create () in
    let instance = {
      emit = (fun msg -> emit (Some msg));
      identity = gen_identity None;
      state = ref None;
      instance_view = component.component_view;
    } in
    (instance, events)

  let emit instance = instance.emit

  (* default implementation: just use a list *)
  module ChildCache = Collection_cache.Make(Collection_cache.Child_list)

  let children ~view ~message ~id instance =
    let open Vdom in
    let emit = wrap_message instance message in
    let cache = ChildCache.init () in
    fun state ->
      ChildCache.use cache (fun get_or_create ->
        state |> List.map (fun state ->
          let id = User_tag (id state) in
          let child = get_or_create id (fun _ -> update_and_view {
            emit = emit;
            identity = id;
            instance_view = view;
            state = ref None;
          }) in
          child state
        )
      )

  let collection ~view ~id instance = children ~view ~id ~message:identity instance

  let child ~view ~message ?id instance =
    let child : ('elt, 'model, 'message) instance = {
      emit = wrap_message instance message;
      identity = gen_identity id;
      instance_view = view;
      state = ref None;
    } in
    update_and_view child

  module Handler = struct
    let wrap fn : 'a -> Html.event_response = fun _ -> fn (); `Handled
    let emit instance message : 'a -> Html.event_response = (fun _ -> emit instance message; `Handled)
  end

  exception Assertion_error of string

  module Diff = struct
    open Vdom
    type vdom = Vdom.node
    type element = Dom_html.element Js.t
    type text_node = Dom.text Js.t
    type any_node = Dom.node Js.t

    type node_target = [ `Target_node of (any_node * element) ]
    type element_target = [ `Target_element of (element * element) ]
    type target = [ element_target | node_target ]

    type child_position = 
      | Append
      | Before of any_node

    let before node = Before (node:>any_node)

    (* dom utils *)

    let force_option = function None -> raise (Assertion_error "force_option") | Some x -> x

    let force_opt v = Js.Opt.get v (fun () -> raise (Assertion_error "force_opt"))

    let force_element_of_node node = Dom_html.CoerceTo.element node |> force_opt

    let remove : [< element_target | node_target ] -> unit = function
        | `Target_node (old, parent) -> Dom.removeChild parent old
        | `Target_element (old, parent) -> Dom.removeChild parent old

    let add_child ~parent (pos:child_position) (child:any_node) : unit =
      Dom.insertBefore parent child (match pos with
        | Append -> Js.null
        | Before next -> Js.some next
      )

    let first_child (element:element) : any_node option =
      element##.firstChild |> Js.Opt.to_option

    let remove_all (parent:element) : unit =
      let rec loop () =
        match first_child parent with
          | None -> ()
          | Some child ->
              remove (`Target_node (child, parent)); loop ()
      in
      loop ()

    let _set_attr (element:element) pair : unit =
      let open Attr in
      match pair with
        | key, Attribute value ->
            element##(setAttribute (Js.string key) (Js.string value))
        | key, Property value ->
            Js.Unsafe.set element (Js.string key) value

    let set_attr (element:element) (key:AttrKey.t) (value:Attr.value) : unit =
      _set_attr element (Attr.canonicalize_pair (key, value))

    let remove_attr (element:element) (key:AttrKey.t) : unit =
      let open AttrKey in
      match key with
        | Attribute_name key -> element##removeAttribute (Js.string key)
        (* NOTE: delete not sufficient, won't e.g. disable a checkbox *)
        | Property_name key -> _set_attr element (key, Attr.Property (Js.Unsafe.inject Js.undefined))


    (* vdom <-> dom functions *)

    let render_text t : text_node =
      Dom_html.document##createTextNode(Js.string t)

    let rec render_element e : element =
      let { e_attrs; e_children; e_name } = e in
      let dom = Dom_html.document##createElement(Js.string e_name) in
      e_attrs |> AttrMap.iter (set_attr dom);
      e_children |> List.iter (fun child ->
        add_child ~parent:dom Append (render child)
      );
      dom

    and render_raw : raw_node -> any_node = function
      | Element e -> (render_element e :> any_node)
      | Text t -> (render_text t :> any_node)

    and render : vdom -> any_node = function
      | Anonymous raw -> render_raw raw
      | Identified (_, raw) -> render_raw raw

    let parent_of_target = function
      | `Target_node (_, parent)
      | `Target_element (_, parent) -> parent

    let node_of_node_target (`Target_node (node, _)) = node

    let only_target_of_parent : element -> node_target = fun parent ->
      `Target_node (first_child parent |> force_option, parent)

    let force_text_node : node_target -> text_node = fun target ->
      let node = node_of_node_target target in
      Dom.CoerceTo.text node |> force_opt

    let force_target_node : target -> node_target = function
      | `Target_node _ as target -> target
      | _ -> raise (Assertion_error "force_target_node")

    let force_target_element : target -> element_target = function
      | `Target_element _ as t -> t
      | `Target_node (node, parent) -> `Target_element (force_element_of_node node, parent)

    let replace_contents ~(target:[<target]) contents =
      Log.debug (fun m -> m "replacing contents");
      let contents = (contents:>any_node) in
      match target with
        (* XXX combine these two patterns? *)
        | `Target_node (old, parent) as target -> (
          add_child ~parent (before old) contents;
          remove target
        )
        | `Target_element (old, parent) as target -> (
          add_child ~parent (before old) contents;
          remove target
        )

    let find_option fn list =
      let rec find = function
        | [] -> None
        | candidate::tail -> (
            match fn candidate with
              | Some _ as rv -> rv
              | None -> find tail
        )
      in
      find list

    let update_attributes previous current (`Target_element (element, _)) : unit =
      let old_attrs = ref previous in
      let new_values = current |> AttrMap.filter (fun key value ->
        let matches_existing_value = try
          AttrMap.find key previous |> Attr.eq value
        with Not_found -> false
        in
        if matches_existing_value then (
          (* skip it *)
          Log.debug (fun m -> m "attr unchanged: %s" (Attr.string_of_attr (key, value)));
          old_attrs := AttrMap.remove key !old_attrs;
          false
        ) else true
      ) in

      (* any old_values that aren't identical in the new view *)
      !old_attrs |> AttrMap.iter (fun key _ ->
        Log.debug (fun m -> m "removing old attr: %s" (Attr.string_of_attr_name key));
        remove_attr element key
      );

      (* Once all the old stuff is gone, add the new values. We need to do
       * this last in case e.g. an attribute switches to a property, in which
       * case unsetting the old value might clobber the new one *)
      Log.debug (fun m ->
        let attrs = new_values |> AttrMap.bindings |> List.map Attr.string_of_attr in
        m "setting new attrs: %s" (String.concat ", " attrs)
      );
      new_values |> AttrMap.iter (set_attr element)

    let invalid_dom () =
      raise (Assertion_error "Invalid DOM state!")

    let nth_child element idx : any_node option =
      element##.childNodes##item(idx) |> Js.Opt.to_option

    let rec update_children previous current (`Target_element (parent, _)) : unit = (
      let previous_remaining = ref previous in
      let force_dom_node idx : any_node = nth_child parent idx |> force_option in

      Log.debug (fun m -> m "processing %d children (currently there are %d)"
        (List.length current)
        (List.length previous)
      );

      current |> List.iteri (fun idx current_child ->
        Log.debug (fun m -> m "processing node %s at idx %d"
          (string_of_node current_child) idx);
        match !previous_remaining with
          | [] -> add_child ~parent Append (render current_child)
          | previous_child :: previous_remaining_tail -> (
            let previous_matching_child = ( match current_child with
              | Identified (current_id, current_node) -> (
                !previous_remaining |> find_option (function
                  | Identified (id, _) as result when Identity.eq id current_id -> Some result
                  | _ -> None
                )
              )
              | Anonymous current_child -> (match (previous_child, current_child) with
                (* Note: we don't do any lookahead for anonymous nodes, chances of a good
                 * match in the face of reordering is slim anyway *)
                | (
                    Anonymous (Element { e_name = previous_element_name ; _ }),
                    (Element { e_name = current_element_name ; _ })
                  ) when previous_element_name = current_element_name ->
                  Log.debug (fun m ->
                    m "found matching element for %s" (string_of_raw current_child));
                  Some (previous_child)
                | Anonymous (Text _), Text _ -> Some (previous_child)
                | _ ->
                  Log.debug (fun m -> m "Existing node is %s, which is not suitable for %s"
                    (string_of_node previous_child)
                    (string_of_raw current_child));
                  None
              )
            ) in
            match previous_matching_child with
              | None -> (* No match found; just insert it *)
                Log.debug (fun m -> m "inserting before existing node at idx %d" idx);
                add_child ~parent (Before (force_dom_node idx)) (render current_child)
              | Some previous_matching_child when previous_matching_child = previous_child ->
                (* no reordering required *)
                Log.debug (fun m ->
                  m "node %s matched existing node %s"
                  (string_of_node current_child)
                  (string_of_node previous_matching_child));

                previous_remaining := previous_remaining_tail;
                update_node previous_matching_child current_child (`Target_node (force_dom_node idx, parent))
              | Some previous_matching_child -> (
                (* we found it further in the list, not at the current element.
                 * Note: we could do better if we rearranged nodes, but right now just
                 * dropping everyone in the way will do well enough *)
                let rec remove_leading_nodes = (function
                  | [] -> failwith "end of list reached in remove_leading_nodes"
                  | candidate :: tail ->
                    if candidate = previous_matching_child
                    then tail
                    else (
                      Log.debug (fun m -> m "removing node %s" (string_of_node candidate));
                      remove (`Target_node (force_dom_node idx, parent));
                      remove_leading_nodes tail
                    )
                ) in
                previous_remaining := remove_leading_nodes !previous_remaining;

                Log.debug (fun m ->
                  m "updating node %s -> %s"
                  (string_of_node previous_matching_child)
                  (string_of_node current_child));

                update_node
                  previous_matching_child
                  current_child
                  (`Target_node (force_dom_node idx, parent))
              )
          )
      );

      let rec remove_trailing_nodes = fun expected idx -> (
        match expected, nth_child parent idx with
          | [], None -> ()
          | vdom_node::expected, Some node ->
            Log.debug (fun m -> m "Removing node at idx %d (for %s)"
              idx (string_of_node vdom_node));
            remove (`Target_node (node, parent));
            remove_trailing_nodes expected idx
          | [], Some _ -> raise (Assertion_error ("Expected no more trailing DOM nodes at idx " ^ (string_of_int idx)))
          | node::_, None -> raise (Assertion_error (
              "Expected a trailing DOM node at idx "
              ^ (string_of_int idx)
              ^ ": for VDOM "
              ^ (string_of_node node)
            ))
      ) in
      Log.debug (fun m ->
        m "Removing %d trailing nodes after updating %d to %d"
        (List.length !previous_remaining)
        (List.length previous)
        (List.length current));
      remove_trailing_nodes !previous_remaining (List.length current);
    )

    and replace_text : text_node -> string -> unit = fun target current ->
      target##.data := (Js.string current)

    and update_element
        ({ e_name = previous_name; e_attrs = previous_attrs; e_children = previous_children } as previous)
        ({ e_name = current_name;  e_attrs = current_attrs;  e_children = current_children  } as current)
        (target:element_target) : unit =
      Log.debug (fun m -> m "updade_element %s -> %s"
        (string_of_element previous)
        (string_of_element current));
      if previous_name <> current_name then
        (* can't change node type, burn it to the ground *)
        replace_contents ~target (render_element current)
      else (
        update_attributes previous_attrs current_attrs target;
        update_children previous_children current_children target
      )

    and update_raw previous current (target:[<target]) = (match previous, current with
      | Element previous, Element current ->
          let target = force_target_element target in
          update_element previous current target
      | _, Element current -> replace_contents ~target (render_element current)
      | Text previous, Text current ->
        if previous <> current then (
          let target = target
            |> force_target_node
            |> force_text_node in
          replace_text target current
        )
      | _, Text current -> replace_contents ~target (render_text current)
    )

    and update_node : vdom -> vdom -> node_target -> unit = fun previous current target -> (
      if previous != current then (
        (* cheap physical inequality, to short-circuit view functions which use a cached value *)
        match (previous, current) with
          | Anonymous p, Anonymous c -> update_raw p c (target:>target)
          | _, Anonymous c -> replace_contents ~target (render_raw c)
          | Identified (pid, p), Identified (cid, c)
              when Identity.eq pid cid -> update_raw p c (target:>target)
          | _, Identified (_, c) -> replace_contents ~target (render_raw c)
      )
    )

    (* used internally to insert errors *)
    let prepend (state:vdom) (parent:element) =
      add_child ~parent (match first_child parent with
        | Some node -> Before node
        | None -> Append
      ) (render state)

    (* Public API: *)
    let init (state:vdom) (parent:element) =
      remove_all parent;
      add_child ~parent Append (render state)

    let update (previous:vdom) (current:vdom) (root:element) =
      Log.debug (fun m -> m "processing new vdom %s" (string_of_node current));
      let target = only_target_of_parent root in
      update_node previous current target

  end

  let render
      (component: ('elt, 'model, 'message) component)
      (root:Dom_html.element Js.t)
      : ('elt, 'model, 'message) instance * unit Lwt.t =
    let instance, events = instantiate component in

    let run_thread = (
      let view_fn = update_and_view instance in
      (* NOTE: we're duplicating state and view here
       * because we can't trust the user not to mess
       * with it by rendering an instance twice *)
      let state = ref component.init in
      let view = ref (view_fn !state) in
      try%lwt
        Diff.init !view root;
        Lwt_stream.iter (fun message ->
          let new_state = component.update !state message in
          let new_view = view_fn new_state in
          Log.info (fun m -> m "Updating view");
          Diff.update !view new_view root;
          state := new_state;
          view := new_view;
        ) events
      with e -> (
        let backtrace = Printexc.get_backtrace () in
        let err = Printexc.to_string e in
        Log.err (fun m -> m "%s\n%s" err backtrace);
        (try
          (* Diff.remove_all root; *)
          Diff.prepend (
            let open Html in
            div [
              h1 [ pcdata "Uncaught error:" ];
              h2 [ pcdata err ];
              pre [ pcdata backtrace ];
              hr ();
            ]
          ) root;
          with _ -> ()
        );
        raise e
      )
    ) in
    (instance, run_thread)


  let onload fn =
    (* Note: this should be Lwt_main.run, but no such thing in JS *)
    let (_:unit Lwt.t) = (Lwt.(Lwt_js_events.onload () >>= (fun _evt -> fn ()))) in
    ()

  let main ?log ?root ?background ui () =
    let () = match log with
      | Some lvl -> set_log_level lvl
      | None -> init_logging ()
    in
    let root = match root with
      | Some root -> root
      | None -> (fun () -> (Dom_html.document##.body))
    in
    let background = match background with
      | Some bg -> bg
      | None -> (fun () -> Lwt.return_unit)
    in
    let instance, run_thread = render ui (root ()) in
    Lwt.join [ background (); run_thread ]

end
