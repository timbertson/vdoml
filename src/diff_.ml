open Log_
open Vdom_
open Attr_

exception Assertion_error of string

module type DOM_HOOKS = sig
  val register_element : Dom_html.element Js.t -> unit
  val unregister_element : Dom_html.element Js.t -> unit
end

module No_hooks : DOM_HOOKS = struct
  let register_element _e = ()
  let unregister_element = register_element
end

module Make(Hooks:DOM_HOOKS) = struct
  open Vdom
  type dom_element = Dom_html.element Js.t
  type dom_text = Dom.text Js.t
  type dom_any = Dom.node Js.t

  type node_target = [ `Target_node of (dom_any * dom_element) ]
  type element_target = [ `Target_element of (dom_element * dom_element) ]
  type target = [ element_target | node_target ]

  type 'msg ctx = {
    emit : 'msg -> unit
  }

  type 'msg state = 'msg ctx * 'msg node

  type child_position = 
    | Append
    | Before of dom_any

  let before node = Before (node:>dom_any)

  (* printers *)

  let string_of_element { e_attrs; e_children; e_name } =
    let attrs = e_attrs |> AttrMap.bindings |> List.map (string_of_attr) |> String.concat " " in
    Printf.sprintf "<%s %s (%d children)>" e_name attrs (List.length e_children)

  let string_of_raw = function
    | Element e -> string_of_element e
    | Text t -> "<#text: " ^ t ^ ">"

  let string_of_node = function
    | Anonymous raw -> string_of_raw raw
    | Identified (_id, raw) -> string_of_raw raw

  (* dom utils *)

  let force_option = function None -> raise (Assertion_error "force_option") | Some x -> x

  let force_opt v = Js.Opt.get v (fun () -> raise (Assertion_error "force_opt"))

  let force_element_of_node node = Dom_html.CoerceTo.element node |> force_opt

  let remove : [< element_target | node_target ] -> unit = function
      | `Target_node (old, parent) -> Dom.removeChild parent old
      | `Target_element (old, parent) ->
        Hooks.unregister_element old;
        Dom.removeChild parent old

  let add_child ~parent (pos:child_position) (child:dom_any) : unit =
    Dom.insertBefore parent child (match pos with
      | Append -> Js.null
      | Before next -> Js.some next
    )

  let first_child (element:dom_element) : dom_any option =
    element##.firstChild |> Js.Opt.to_option

  let remove_all (parent:dom_element) : unit =
    let rec loop () =
      match first_child parent with
        | None -> ()
        | Some child ->
            remove (`Target_node (child, parent)); loop ()
    in
    loop ()

  let set_attr ctx (element:dom_element) (key:AttrKey.t) (value:'msg Vdom.attr) : unit =
    let open Attr in
    match key, value with
      | AttrKey.Attribute_name key, Attribute value ->
          element##(setAttribute (Js.string key) (Js.string value))
      | AttrKey.Property_name key, Property value ->
          Js.Unsafe.set element (Js.string key) (Vdom.js_of_property ctx.emit value)
      | _ -> failwith "impossible!"

  let remove_attr (element:dom_element) (key:AttrKey.t) : unit =
    let open AttrKey in
    match key with
      | Attribute_name key -> element##removeAttribute (Js.string key)
      (* NOTE: delete not sufficient, won't e.g. disable a checkbox *)
      | Property_name key -> Js.Unsafe.set element (Js.string key) (Js.undefined)

  (* vdom <-> dom functions *)

  let render_text t : dom_text =
    Dom_html.document##createTextNode(Js.string t)

  let rec render_element ctx (e:'msg element) : dom_element =
    Log.info (fun m->m "rendering element: %s" (string_of_element e));
    let { e_attrs; e_children; e_name } = e in
    let dom = Dom_html.document##createElement(Js.string e_name) in
    e_attrs |> AttrMap.iter (set_attr ctx dom);
    e_children |> List.iter (fun child ->
      add_child ~parent:dom Append (render ctx child)
    );
    Hooks.register_element dom;
    dom

  and render_raw ctx (node:'msg raw_node) : dom_any = match node with
    | Element e -> (render_element ctx e :> dom_any)
    | Text t -> (render_text t :> dom_any)

  and render ctx : 'msg node -> dom_any = function
    | Anonymous raw -> render_raw ctx raw
    | Identified (_, raw) -> render_raw ctx raw

  let parent_of_target = function
    | `Target_node (_, parent)
    | `Target_element (_, parent) -> parent

  let node_of_node_target (`Target_node (node, _)) = node

  let only_target_of_parent : dom_element -> node_target = fun parent ->
    `Target_node (first_child parent |> force_option, parent)

  let force_text_node : node_target -> dom_text = fun target ->
    let node = node_of_node_target target in
    Dom.CoerceTo.text node |> force_opt

  let force_target_node : target -> node_target = function
    | `Target_node _ as target -> target
    | _ -> raise (Assertion_error "force_target_node")

  let force_target_element : target -> element_target = function
    | `Target_element _ as t -> t
    | `Target_node (node, parent) -> `Target_element (force_element_of_node node, parent)

  let replace_contents ~(target:[<target]) contents =
    Log.info (fun m -> m "replacing contents");
    let contents = (contents:>dom_any) in
    let replace (old: #Dom.node Js.t) parent =
      add_child ~parent (before old) contents;
      remove target
    in
    match target with
      | `Target_node (old, parent) -> replace old parent
      | `Target_element (old, parent) -> replace old parent

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

  let update_attributes ctx previous current (`Target_element (element, _)) : unit =
    let old_attrs = ref previous in
    let new_values = current |> AttrMap.filter (fun key value ->
      let matches_existing_value = try
        AttrMap.find key previous |> attr_eq value
      with Not_found -> false
      in
      if matches_existing_value then (
        (* skip it *)
        Log.debug (fun m -> m "attr unchanged: %s" (string_of_attr (key, value)));
        old_attrs := AttrMap.remove key !old_attrs;
        false
      ) else true
    ) in

    (* any old_values that aren't identical in the new view *)
    !old_attrs |> AttrMap.iter (fun key _ ->
      Log.info (fun m -> m "removing old attr: %s" (Attr.string_of_attr_name key));
      remove_attr element key
    );

    (* Once all the old stuff is gone, add the new values. We need to do
     * this last in case e.g. an attribute switches to a property, in which
     * case unsetting the old value might clobber the new one *)
    if not (AttrMap.is_empty new_values) then (
      Log.info (fun m ->
        let attrs = new_values |> AttrMap.bindings |> List.map string_of_attr in
        m "setting new attrs: %s" (String.concat ", " attrs)
      );
    );
    new_values |> AttrMap.iter (set_attr ctx element)

  let invalid_dom () =
    raise (Assertion_error "Invalid DOM state!")

  let nth_child element idx : dom_any option =
    element##.childNodes##item(idx) |> Js.Opt.to_option

  let rec update_children ctx (previous:'msg node list) (current:'msg node list) (`Target_element (parent, _)) : unit = (
    let previous_remaining = ref previous in
    let force_dom_node idx : dom_any = nth_child parent idx |> force_option in

    Log.debug (fun m -> m "processing %d children (currently there are %d)"
      (List.length current)
      (List.length previous)
    );

    current |> List.iteri (fun idx current_child ->
      Log.debug (fun m -> m "processing node %s at idx %d"
        (string_of_node current_child) idx);
      match !previous_remaining with
        | [] -> add_child ~parent Append (render ctx current_child)
        | previous_child :: previous_remaining_tail -> (
          let previous_matching_child = (match current_child with
            | Identified (current_id, _) -> (
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
              Log.info (fun m -> m "inserting before existing node at idx %d" idx);
              add_child ~parent (Before (force_dom_node idx)) (render ctx current_child)
            | Some previous_matching_child when previous_matching_child == previous_child ->
              (* no reordering required *)
              Log.debug (fun m ->
                m "node %s matched existing node %s"
                (string_of_node current_child)
                (string_of_node previous_matching_child));

              previous_remaining := previous_remaining_tail;
              update_node ctx previous_matching_child current_child (`Target_node (force_dom_node idx, parent))
            | Some previous_matching_child -> (
              (* we found it further in the list, not at the current element.
               * Note: we could do better if we rearranged nodes, but right now just
               * dropping everyone in the way will do well enough *)
              let rec remove_leading_nodes = (function
                | [] -> failwith "end of list reached in remove_leading_nodes"
                | candidate :: tail ->
                  if candidate == previous_matching_child
                  then tail
                  else (
                    Log.info (fun m -> m "removing node %s" (string_of_node candidate));
                    remove (`Target_node (force_dom_node idx, parent));
                    remove_leading_nodes tail
                  )
              ) in
              previous_remaining := remove_leading_nodes !previous_remaining;

              Log.debug (fun m ->
                m "updating node %s -> %s"
                (string_of_node previous_matching_child)
                (string_of_node current_child));

              update_node ctx
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
          Log.info (fun m -> m "Removing node at idx %d (for %s)"
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

  and replace_text : dom_text -> string -> unit = fun target current ->
    target##.data := (Js.string current)

  and update_element ctx
      ({ e_name = previous_name; e_attrs = previous_attrs; e_children = previous_children } as previous)
      ({ e_name = current_name;  e_attrs = current_attrs;  e_children = current_children  } as current)
      (target:element_target) : unit =
    Log.debug (fun m -> m "updade_element %s -> %s"
      (string_of_element previous)
      (string_of_element current));
    if previous_name <> current_name then
      (* can't change node type, burn it to the ground *)
      replace_contents ~target (render_element ctx current)
    else (
      update_attributes ctx previous_attrs current_attrs target;
      update_children ctx previous_children current_children target
    )

  and update_raw ctx previous current (target:[<target]) = (match previous, current with
    | Element previous, Element current ->
        let target = force_target_element target in
        update_element ctx previous current target
    | _, Element current -> replace_contents ~target (render_element ctx current)
    | Text previous, Text current ->
      if previous <> current then (
        let target = target
          |> force_target_node
          |> force_text_node in
        replace_text target current
      )
    | _, Text current -> replace_contents ~target (render_text current)
  )

  and update_node : 'msg ctx -> 'msg node -> 'msg node -> node_target -> unit = fun ctx previous current target -> (
    if previous != current then (
      (* cheap physical inequality, to short-circuit view functions which use a cached value *)
      match (previous, current) with
        | Anonymous p, Anonymous c -> update_raw ctx p c (target:>target)
        | _, Anonymous c -> replace_contents ~target (render_raw ctx c)
        | Identified (pid, p), Identified (cid, c)
            when Identity.eq pid cid -> update_raw ctx p c (target:>target)
        | _, Identified (_, c) -> replace_contents ~target (render_raw ctx c)
    )
  )

  (* used internally to insert errors *)
  let prepend ~emit (state:'msg html) (parent:dom_element) =
    let ctx = { emit } in
    add_child ~parent (match first_child parent with
      | Some node -> Before node
      | None -> Append
    ) (render ctx (Internal.root state))

  (* Public API: *)
  let init ~emit (current:'msg pure_node) (parent:dom_element) =
    let ctx = { emit } in
    remove_all parent;
    let current = Internal.root current in
    add_child ~parent Append (render ctx current);
    (ctx, current)

  let update : 'msg state -> 'msg html -> dom_element -> 'msg state = fun (ctx, previous) current root ->
    let current = Internal.root current in
    Log.debug (fun m -> m "processing new vdom %s" (string_of_node current));
    let target = only_target_of_parent root in
    update_node ctx previous current target;
    (ctx, current)
end
