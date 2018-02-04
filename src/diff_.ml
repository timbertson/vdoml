open Log_
open Vdom_
open Attr_
open Util_
open Event_chain_ (* TODO: Conversion_chain_ *)

module type DOM_HOOKS = sig
  val on_create : Dom_html.element Js.t -> unit
  val on_destroy : Dom_html.element Js.t -> unit
end

module No_hooks : DOM_HOOKS = struct
  let on_create _e = ()
  let on_destroy = on_create
end

module Make(Hooks:DOM_HOOKS) = struct
  open Vdom
  type dom_element = Dom_html.element Js.t
  type dom_text = Dom.text Js.t
  type dom_any = Dom.node Js.t

  type node_target = [ `Target_node of (dom_any * dom_element) ]
  type element_target = [ `Target_element of (dom_element * dom_element) ]
  type target = [ element_target | node_target ]

  type 'msg ctx = 'msg Event_chain.t

  let emit : 'msg ctx -> 'msg -> unit = fun ctx msg ->
    Event_chain.emit ctx msg

  type 'msg state = 'msg ctx * 'msg node

  type child_position =
    | Append
    | Before of dom_any

  let before node = Before (node:>dom_any)

  let as_dom_any el = (el:>dom_any)

  (* printers *)

  let string_of_element { e_attrs; e_children; e_name } =
    let attrs = e_attrs |> AttrMap.bindings |> List.map (Attr.string_of_attr) |> String.concat " " in
    Printf.sprintf "<%s %s (%d children)>" e_name attrs (List.length e_children)

  let string_of_text t = "<#text: " ^ t ^ ">"

  let rec string_of_content_node : type msg. msg content_node -> string = function
    | Element el -> string_of_element el
    | Text t -> string_of_text t

  let rec string_of_node : type msg. msg node -> string = function
    | Observer (_, node) -> "Observer(" ^ (string_of_node node) ^ ")"
    | Conversion (_, node) -> "Conversion(" ^ (string_of_node node) ^ ")"
    | Identity (id, node) -> "Identity(" ^ (Identity.to_string id) ^ ", " ^ (string_of_node node) ^ ")"
    | Content c -> string_of_content_node c

  (* dom utils *)

  let force_option = function None -> raise (Assertion_error "force_option") | Some x -> x

  let force_opt v = Js.Opt.get v (fun () -> raise (Assertion_error "force_opt"))

  let force_element_of_node node = Dom_html.CoerceTo.element node |> force_opt

  let invalid_dom () =
    raise (Assertion_error "Invalid DOM state!")

  let nth_child element idx : dom_any option =
    element##.childNodes##item(idx) |> Js.Opt.to_option

  let parent_of_target = function
    | `Target_node (_, parent)
    | `Target_element (_, parent) -> parent

  let node_of_node_target (`Target_node (node, _)) = node

  let node_of_target : [<target] -> dom_any = function
    | (`Target_node (node, _)) -> node
    | (`Target_element (node, _)) -> (node:>dom_any)

  let first_child (element:dom_element) : dom_any option =
    element##.firstChild |> Js.Opt.to_option

  let only_target_of_parent : dom_element -> node_target = fun parent ->
    `Target_node (first_child parent |> force_option, parent)

  let force_text_node : node_target -> dom_text = fun target ->
    let node = node_of_node_target target in
    Dom.CoerceTo.text node |> force_opt

  let force_target_node : target -> node_target = function
    | `Target_node _ as target -> target
    | _ -> raise (Assertion_error "force_target_node")

  let force_target_element : [<target] -> element_target = function
    | `Target_element _ as t -> t
    | `Target_node (node, parent) -> `Target_element (force_element_of_node node, parent)

  let run_hook element = function
    | None -> ()
    | Some hook -> hook element

  let rec has_removal_hooks: type msg. msg node -> bool = function
    | Content c -> has_removal_hooks_content c
    | Conversion (_, n) -> has_removal_hooks n
    | Observer (_, n) -> has_removal_hooks n
    | Identity (_, n) -> has_removal_hooks n

  (* Note: extraction of gadt needs to be annotated very carefully:
     https://gist.github.com/infinity0/ea064ba358ea44e4f4919789f12d8d7e
  *)
  and has_removal_hooks_content: type msg. msg content_node -> bool = function
    | Element el -> has_removal_hooks_elem el
    | Text _ -> false

  and has_removal_hooks_elem: type msg. msg element -> bool = fun element ->
    match element.e_hooks.hook_destroy with
    | Some _ -> true
    | None -> element.e_children |> List.any has_removal_hooks

  and call_removal_hooks_elem : type msg. msg element -> dom_any -> unit = fun elem node -> (
    let node = force_element_of_node node in
    elem.e_children |> List.iteri (fun i child ->
      match nth_child node i with
        | Some dom_child -> call_removal_hooks child dom_child
        | None -> invalid_dom ()
    );
    match elem.e_hooks.hook_destroy with
      | Some hook -> hook node
      | None -> ()
  )

  and call_removal_hooks : type msg. msg node -> dom_any -> unit = fun content target ->
    match content with
      | Content (Element e) -> call_removal_hooks_elem e target
      | Content (Text _) -> ()
      | Conversion (_, n) -> call_removal_hooks n target
      | Observer (_, n) -> call_removal_hooks n target
      | Identity (_, n) -> call_removal_hooks n target

  let remove_dom (target:[< element_target | node_target ]) =
    let vdom_element, node, parent = match target with
      | `Target_element (old, parent) -> (Some old, (old:>dom_any), parent)
      | `Target_node (old, parent) ->
        (Dom_html.CoerceTo.element old |> Js.Opt.to_option, old, parent)
    in
    vdom_element |> Option.may Hooks.on_destroy;
    Dom.removeChild parent node

  let remove_element : 'msg element -> [<target] -> unit = fun vdom node ->
    (* Removal hooks are rare, but invoking them requires full DOM traversal. Check
     * if any exist in this subtree before bothering with the DOM. *)
    if has_removal_hooks_elem vdom then (
      call_removal_hooks_elem vdom (node_of_target node)
    );
    remove_dom node

  let rec identity_of_node : type msg. msg node -> identity option = function
    | Conversion (_, node) -> identity_of_node node
    | Observer (_, node) -> identity_of_node node
    | Identity (id, _) -> Some id
    | Content c -> None

  let remove_content_node content target = match content with
      | Element e -> remove_element e target
      | Text t -> remove_dom target

  let rec remove : type msg. msg node -> [<target] -> unit = fun vdom target ->
    match vdom with
      | Conversion (_, node) -> remove node target
      | Observer (fn, node) -> remove node target
      | Identity (id, node) -> remove node target
      | Content c -> remove_content_node c target

  let add_child ~parent (pos:child_position) (child:dom_any) : unit =
    Dom.insertBefore parent child (match pos with
      | Append -> Js.null
      | Before next -> Js.some next
    )

  let remove_all_dom (parent:dom_element) : unit =
    let rec loop () =
      match first_child parent with
        | None -> ()
        | Some child ->
            remove_dom (`Target_node (child, parent)); loop ()
    in
    loop ()

  let set_property_if_changed =
    (* Ocaml doesn't expose easy access to the `===`
     * operator, so implement this in JS:
     *)
    let open Js.Unsafe in
    let fn = js_expr ("(
      function(elem, key, value) {
        if (elem[key] !== value) {
          elem[key] = value;
        }
      })") in
    fun elem key value ->
      fun_call fn [|inject elem; inject key; inject value|]

  let set_attr ctx (element:dom_element) (key:Attr.key) (value:'msg Attr.value) : unit =
    let open Attr in
    match key, value with
      | AttrKey.Attribute_name key, Attribute (String_attr value) ->
        element##(setAttribute (Js.string key) (Js.string value))

      | AttrKey.Attribute_name key, Attribute (Dynamic_attr hook) ->
        hook element key

      | AttrKey.Property_name key, Property value ->
        set_property_if_changed element (Js.string key) (Attr.js_of_property (emit ctx) value)

      | AttrKey.Attribute_name _, Property _
      | AttrKey.Property_name _, Attribute _
        -> failwith "impossible!"

  let remove_attr (element:dom_element) (key:AttrKey.t) : unit =
    let open AttrKey in
    match key with
      | Attribute_name key -> element##removeAttribute (Js.string key)
      (* NOTE: delete not sufficient, won't e.g. disable a checkbox *)
      | Property_name key -> Js.Unsafe.set element (Js.string key) (Js.undefined)

  (* vdom <-> dom functions *)

  let render_text t : dom_text =
    Dom_html.document##createTextNode(Js.string t)

  let rec render_element: type msg. msg ctx -> msg element -> dom_element = fun ctx e ->
    Log.info (fun m->m "rendering element: %s" (string_of_element e));
    let { e_attrs; e_children; e_name; e_hooks } = e in

    let dom = Dom_html.document##createElement(Js.string e_name) in
    e_attrs |> AttrMap.iter (set_attr ctx dom);
    e_children |> List.iter (fun child ->
      add_child ~parent:dom Append (render ctx child)
    );
    Hooks.on_create dom;
    run_hook dom e_hooks.hook_create;
    dom

  and render_content_node: type msg. msg ctx -> msg content_node -> dom_any = fun ctx -> function
    | Text text -> render_text text |> as_dom_any
    | Element el -> render_element ctx el |> as_dom_any

  and render: type msg. msg ctx -> msg node -> dom_any = fun ctx -> function
    | Content c -> render_content_node ctx c
    | Identity (id, node) -> render ctx node

    | Conversion (fn, node) ->
      render (Event_chain.add_conversion fn ctx) node

    | Observer (fn, node) ->
      render (Event_chain.add_observer fn ctx) node

  let replace_contents ~(target:[<target]) ~existing contents: unit =
    Log.info (fun m -> m "replacing contents of %s" (string_of_node existing));
    let contents = (contents:>dom_any) in
    let replace (old: #Dom.node Js.t) parent =
      add_child ~parent (before old) contents;
      remove existing target
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

  let update_attributes ctx (existing: 'msg Attr.map) (current: 'msg Attr.map) (`Target_element (element, _)) : unit =
    let old_attrs = ref existing in
    let new_values = current |> AttrMap.filter (fun key value ->
      old_attrs := AttrMap.remove key !old_attrs;
      let matches_existing_value = try
        AttrMap.find key existing |> Attr.eq value
      with Not_found -> false
      in
      if matches_existing_value then (
        (* skip it *)
        Log.debug (fun m -> m "attr unchanged: %s" (Attr.string_of_attr (key, value)));
        false
      ) else true
    ) in

    (* any old_values that aren't present in the new view *)
    !old_attrs |> AttrMap.iter (fun key _ ->
      Log.info (fun m -> m "removing old attr: %s" (Attr.string_of_attr_name key));
      remove_attr element key
    );

    (* Once all the old stuff is gone, add the new values. We need to do
     * this last because in theory a new property and old attribute could
     * refer to the same underlying property *)
    if not (AttrMap.is_empty new_values) then (
      Log.info (fun m ->
        let attrs = new_values |> AttrMap.bindings |> List.map Attr.string_of_attr in
        m "setting new attrs: %s" (String.concat ", " attrs)
      );
    );
    new_values |> AttrMap.iter (set_attr ctx element)

  (* only used to track child node mutations *)
  type 'msg node_mutation =
    | Update of 'msg ctx * 'msg node * 'msg node
    | Insert of 'msg ctx * 'msg node
    | Append of 'msg ctx * 'msg node
    | Remove of 'msg node

  type 'msg mutation_state = {
    excess_new_nodes: int;
    dom_idx: int;
    remaining_nodes: 'msg node list; (* TODO: rename remaining_existing *)
  }

  type 'msg mutation_result =
    | Applied of 'msg mutation_state
    | Unapplied of 'msg mutation_state (* unapplied mutations can still alter the state *)

  let string_of_node_mutation =
    function
      | Update (_, a, b ) -> "Update (" ^ (string_of_node a) ^ ", " ^ (string_of_node b) ^ ")"
      | Insert (_, a) -> "Insert " ^ (string_of_node a)
      | Append (_, a) -> "Append " ^ (string_of_node a)
      | Remove a -> "Remove " ^ (string_of_node a)

  (* tails a list, ignoring empty *)
  let drop_one = function | [] -> [] | _head::tail -> tail

  (* Actual DOM manipulation. Substituable for a dummy in test code,
   * leaving the rest of the code pure *)
  let rec apply_child_mutation
    : type msg. parent:dom_element ->
      (msg node_mutation -> msg mutation_state -> unit)
    = fun ~parent -> (
    let force_dom_node idx : dom_any = nth_child parent idx |> force_option in
    fun (mutation:msg node_mutation) { dom_idx; _ } -> (
      match mutation with
        | Update (ctx, existing, replacement) ->
          Log.debug (fun m ->
            m "node %s matched existing node %s"
            (string_of_node replacement)
            (string_of_node existing));
          update_node ctx existing replacement
            (`Target_node (force_dom_node dom_idx, parent))
        | Insert (ctx, replacement) ->
          Log.info (fun m -> m "inserting before existing node at idx %d" dom_idx);
          add_child ~parent (Before (force_dom_node dom_idx)) (render ctx replacement)
        | Append (ctx, replacement) ->
          add_child ~parent Append (render ctx replacement)
        | Remove existing ->
          Log.info (fun m -> m "removing node %s" (string_of_node existing));
          remove existing (`Target_node (force_dom_node dom_idx, parent))
    )
  )

  and lift_mutator : type msg.
    (msg node_mutation -> msg mutation_state -> unit) ->
    (msg node_mutation -> msg mutation_state -> msg mutation_state)
  = fun apply_mutation -> (
    fun mutation ({ excess_new_nodes; dom_idx; remaining_nodes } as state) ->
      Log.debug (fun m->m"applying mutation: %s" (string_of_node_mutation mutation));
      apply_mutation mutation state;
      match mutation with
      | Update (_, existing, replacement) ->
        {
          excess_new_nodes;
          dom_idx = dom_idx + 1;
          remaining_nodes = drop_one remaining_nodes;
        }
      | Insert (ctx, node) ->
        {
          excess_new_nodes = excess_new_nodes - 1;
          dom_idx = dom_idx + 1;
          remaining_nodes;
        }
      | Append (ctx, node) ->
        {
          excess_new_nodes = excess_new_nodes - 1;
          (* Incrementing dom_idx puts us _at_ the new node, not after it.
           * Future modifications must also be Append *)
          dom_idx = dom_idx + 1;
          remaining_nodes;
        }
      | Remove _ ->
        {
          excess_new_nodes = excess_new_nodes + 1;
          dom_idx;
          remaining_nodes = drop_one remaining_nodes;
        }
  )

  and update_children :
    type msg. ?apply_mutation:(msg node_mutation -> msg mutation_state -> unit) ->
    msg ctx -> msg node list -> msg node list -> element_target -> unit
  = fun ?apply_mutation ctx existing replacements (`Target_element (parent, _)) -> (
    let apply = match apply_mutation with
      | Some app -> app
      | None -> apply_child_mutation ~parent
    in
    update_children_pure ~apply ctx existing replacements
  )

  and update_children_pure : type msg.
    apply:(msg node_mutation -> msg mutation_state -> unit)
    -> msg ctx -> msg node list -> msg node list -> unit
  = fun ~apply ctx existing replacements -> (

    let applied state = Applied state in
    let unapplied state = Unapplied state in

    let apply = lift_mutator apply in
    Log.debug (fun m -> m "processing %d children (currently there are %d)"
      (List.length replacements)
      (List.length existing)
    );

    let find_identified ~skipmax ~id nodes =
      let rec next ~offset ~skipmax = function
        | [] -> None
        | node :: tail -> (
          match identity_of_node node |> Option.map (Identity.eq id) with
            | None -> next ~offset:(offset+1) ~skipmax tail
            | Some true -> Some (node, offset)
            | Some false -> (
              if skipmax <= 0
                then None
                else next ~offset:(offset+1) ~skipmax:(skipmax-1) tail
            )
        )
      in
      next ~offset:0 ~skipmax nodes
    in

    let compatible_anon_element ~existing replacement =
      existing.e_name = replacement.e_name
    in

    let rec compatible_anon_node_vs_node : type msg.
      existing: msg node -> msg node -> bool
    = fun ~existing replacement -> (
      match (existing, replacement) with
        | Conversion existing, Conversion replacement -> (
          match cast_mapped_node (Event_chain.emitter ignore) existing replacement with
            | Some (_, existing, replacement) ->
              compatible_anon_node_vs_node ~existing replacement
            | None -> false
        )

        | Observer (fna, existing), Observer (fnb, replacement) ->
          fna == fnb && compatible_anon_node_vs_node ~existing replacement

        | Content (Element existing), Content (Element replacement) ->
          compatible_anon_element ~existing replacement
        
        | Content (Text _), Content (Text _) -> true

        | Content (Text _), _
        | Content (Element _), _
        | Identity _, _
        | Conversion _, _
        | Observer _, _
          -> false
    ) in

    let rec search_and_replace_anon :
      existing:msg node ->
      replacement:msg node ->
      msg mutation_state -> msg mutation_result
    = fun ~existing ~replacement state -> (
      if state.excess_new_nodes > 0 then (
        (* assume this is a new node *)
        apply (Insert (ctx, replacement)) state |> applied
      ) else if state.excess_new_nodes < 0 then (
        (* Assume it got deleted. Remove current and try again *)
        state |> apply (Remove existing) |> unapplied
      ) else (
        (* no hint as to whether there are added or removed nodes.
         * Try a simple lookahead of 1 *)
        let rec lookahead state = (
          Log.debug (fun m -> m "Lookahead at DOM idx %d" state.dom_idx);
          match state.remaining_nodes with
            | [] -> raise (Assertion_error "unexpected end of existing elements")
            | _existing::[] ->
              assert (existing == _existing);
              (* this was the last element *)
              state
                |> apply (Remove (existing))
                |> apply (Append (ctx, replacement))
                |> applied

            | _existing::next_candidate::_ ->
              assert (existing == _existing);
              if compatible_anon_node_vs_node ~existing:next_candidate replacement then (
                (* lookahead found a match; skip an element and update the next *)
                state
                  |> apply (Remove existing)
                  |> apply (Update (ctx, next_candidate, replacement))
                  |> applied
              ) else (
                (* no match, just insert the new node. This will lead to a positive
                 * excess_new_nodes so the next `replacement` will effectively get
                 * a look-behind of 1 *)
                state
                  |> apply (Insert (ctx, replacement))
                  |> applied
              )
        ) in
        lookahead state
      )
    )

    and process_node_both_anon :
      existing:(msg node) ->
      replacement:(msg node) ->
      msg mutation_state ->
      msg mutation_result
    = fun ~existing ~replacement state -> (
      if compatible_anon_node_vs_node ~existing replacement then (
        Log.debug (fun m ->
          m "found matching element for %s" (string_of_node replacement));
        state
          |> apply (Update (ctx, existing, replacement))
          |> applied
      ) else (
        Log.debug (fun m -> m "Existing node is %s, which is not suitable for %s"
          (string_of_node existing)
          (string_of_node replacement));
        search_and_replace_anon ~existing ~replacement state
      )
    )

    and process_node_identified ~existing ~id ~replacement state = (
      Log.debug (fun m -> m "processing identified node %s at idx %d"
        (string_of_node replacement) state.dom_idx);
      (match find_identified ~skipmax:1 ~id state.remaining_nodes with
        | Some (existing, num_skipped) ->
          state
            |> remove_many num_skipped
            |> apply (Update (ctx, existing, replacement))
        | None ->
          (* no identified node found nearby for updating, just insert *)
          apply (Insert (ctx, replacement)) state
      ) |> applied
    )

    and process_node_anon ~existing ~replacement replacements state = (
      match identity_of_node existing with
        | Some id ->
          (match find_identified ~skipmax:0 ~id replacements with
            | Some _ ->
              (* we'll be using this node in the future *)
              state
                |> apply (Insert (ctx, replacement))
                |> applied
            | None ->
              (* doesn't look like this node is still in use.
               * remove it and try again *)
              state
                |> apply (Remove (existing))
                |> unapplied
          )
        | None ->
          process_node_both_anon ~existing ~replacement state
    )

    and remove_many n state = match n with
      | 0 -> state
      | n ->
        state
          |> apply (Remove (List.hd state.remaining_nodes))
          |> remove_many (n-1)

    and process_node ~existing ~replacement replacements state = (
      match identity_of_node replacement with
        | Some id -> process_node_identified ~existing ~id ~replacement state
        | None -> process_node_anon ~existing ~replacement replacements state
    )

    and process : replacements:(msg node list) -> msg mutation_state -> msg mutation_state =
    fun ~replacements state ->
      match state.remaining_nodes with
        | [] ->
          let append : msg mutation_state -> msg node -> msg mutation_state =
            fun state node -> apply (Append (ctx, node)) state
          in
          List.fold_left append state replacements

        | existing :: _ -> (match replacements with
          | [] -> state (* no more replacement nodes to process *)
          | replacement :: remaining_replacements -> (
            let (state, replacements) = match process_node ~existing ~replacement replacements state with
              | Applied state -> (state, remaining_replacements)
              | Unapplied state -> (state, replacements)
            in
            process ~replacements state
          )
        )
    in

    (* end of definitions *)

    let state = {
      excess_new_nodes = (List.length replacements) - (List.length existing);
      dom_idx = 0;
      remaining_nodes = existing;
    } in

    let state = process ~replacements state in

    (* remove excess *)
    let remove state node = apply (Remove node) state in
    let state = List.fold_left remove state state.remaining_nodes in

    (* sanity check all nodes were processed *)
    assert (state.remaining_nodes = []);
  )

  and replace_text : dom_text -> string -> unit = fun target replacement ->
    target##.data := (Js.string replacement)

  and update_element
    : type msg. msg ctx -> msg element -> msg element -> element_target -> unit =
    fun ctx
      ({ e_name = existing_name; e_attrs = existing_attrs; e_children = existing_children } as existing)
      ({ e_name = replacement_name;  e_attrs = replacement_attrs;  e_children = replacement_children  } as replacement)
      target
    ->
    Log.debug (fun m -> m "updade_element %s -> %s"
      (string_of_element existing)
      (string_of_element replacement));
    if existing_name <> replacement_name then
      (* can't change node type, burn it to the ground *)
      replace_contents
        ~target
        ~existing:(node_of_element existing)
        (render_element ctx replacement)
    else (
      update_attributes ctx existing_attrs replacement_attrs target;
      update_children ctx existing_children replacement_children target
    )

  and update_text ~existing ~replacement (target:[<target]) =
    if existing <> replacement then (
      let target = target
        |> force_target_node
        |> force_text_node in
      replace_text target replacement
    )

  and update_node : type msg. msg ctx -> msg node -> msg node -> node_target -> unit = fun ctx existing replacement target -> (
    if existing != replacement then (
      (* cheap physical inequality, to short-circuit view functions which use a cached value *)
      match (existing, replacement) with
        | Observer (existing_fn, existing), Observer (replacement_fn, replacement)
          when existing_fn == replacement_fn ->
            update_node (Event_chain.add_observer replacement_fn ctx) existing replacement target

        | Conversion c_existing, Conversion c_replacement -> (
          match cast_mapped_node ctx c_existing c_replacement with
            | Some (ctx, existing, replacement) -> update_node ctx existing replacement target
            | None -> replace_contents ~target ~existing (render ctx replacement)
        )

        | Content (Element existing), Content (Element replacement) ->
          let target = force_target_element target in
          update_element ctx existing replacement target

        | Content (Text existing), Content (Text replacement) ->
          update_text ~existing ~replacement (target:>target)

        | Identity (_, existing), Identity (_, replacement) ->
          update_node ctx existing replacement target

        | Content _, _
        | Conversion _, _
        | Observer _, _
        | Identity _, _
          -> (
          Log.debug (fun m->m"update_node: no match between %s and %s" (string_of_node existing) (string_of_node replacement));
          replace_contents ~target ~existing (render ctx replacement)
        )
    )
  )

  let make_ctx emit = Event_chain.emitter emit

  (* used internally to insert errors *)
  let prepend ~emit (state:'msg html) (parent:dom_element) =
    let ctx = make_ctx emit in
    add_child ~parent (match first_child parent with
      | Some node -> Before node
      | None -> Append
    ) (render ctx state)

  (* Module-external API: *)
  let init ~emit (current:'msg html) (parent:dom_element) =
    let ctx = make_ctx emit in
    remove_all_dom parent;
    Log.debug (fun m -> m "rendering initial vdom %s" (string_of_node current));
    add_child ~parent Append (render ctx current);
    (ctx, current)

  let update : 'msg state -> 'msg html -> dom_element -> 'msg state
  = fun (ctx, existing) replacement root ->
    Log.debug (fun m -> m "processing new vdom %s" (string_of_node replacement));
    let target = only_target_of_parent root in
    update_node ctx existing replacement target;
    (ctx, replacement)
end
