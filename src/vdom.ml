module Attr_intf = struct
  module type S = sig

    (** This type covers both properties and attributes, despite the name. *)
    type pair

    (** [create name value] creates a simple string-only attribute *)
    val attribute : string -> string -> pair

    (** [string_property name value] creates a simple string-only property *)
    val string_property : string -> string -> pair

    (** [property name value] creates a property with a generic value *)
    val property : string -> Js.Unsafe.any -> pair

    val on
      : string
      -> ('e Dom.event Js.t -> unit)
      -> pair

    val class_    : string -> pair
    val classes   : string list -> pair
    val id        : string -> pair
    val style     : (string * string) list -> pair
    val style_css : string -> pair

    val on_focus  : (Dom_html.event Js.t -> unit) -> pair
    val on_blur   : (Dom_html.event Js.t -> unit) -> pair

    (** [on_input] fires every time the input changes, e.g., whenever a key is pressed in
        the input field.  The current contents are returned as an OCaml string as a
        convenience *)
    val on_input  : (Dom_html.inputElement Js.t -> string -> unit) -> pair

    (** [on_change] fires when the input is complete, e.g., when enter is pressed in the
        input field.  The current contents are returned as an OCaml string as a
        convenience *)
    val on_change : (Dom_html.inputElement Js.t -> string -> unit) -> pair

    val on_click      : (Dom_html.mouseEvent Js.t -> unit) -> pair
    val on_mousemove  : (Dom_html.mouseEvent Js.t -> unit) -> pair
    val on_mouseup    : (Dom_html.mouseEvent Js.t -> unit) -> pair
    val on_mousedown  : (Dom_html.mouseEvent Js.t -> unit) -> pair
    val on_mouseenter : (Dom_html.mouseEvent Js.t -> unit) -> pair
    val on_mouseleave : (Dom_html.mouseEvent Js.t -> unit) -> pair
    val on_mouseover  : (Dom_html.mouseEvent Js.t -> unit) -> pair
    val on_mouseout   : (Dom_html.mouseEvent Js.t -> unit) -> pair

    val on_keyup    : (Dom_html.keyboardEvent Js.t -> unit) -> pair
    val on_keypress : (Dom_html.keyboardEvent Js.t -> unit) -> pair
    val on_keydown  : (Dom_html.keyboardEvent Js.t -> unit) -> pair
  end


end

module AttrKey = struct
  type t = Property_name of string | Attribute_name of string
  let compare : t -> t -> int = Pervasives.compare
end
module AttrMap = Map.Make(AttrKey)

module Attr : sig
    type key = AttrKey.t
    type value =
      | Property of Js.Unsafe.any
      | Attribute of string
    include Attr_intf.S with type pair = key * value
    val list_to_obj : pair list -> < > Js.t
    val list_to_attrs : pair list -> value AttrMap.t
    val eq : value -> value -> bool
    val canonicalize_pair : pair -> (string * value)
end = struct
  type key = AttrKey.t
  type value = 
    | Property of Js.Unsafe.any
    | Attribute of string
  type pair = key * value

  let eq a b = match (a,b) with
    | Property a, Property b -> a == b (* pysical equality for JS any, because I don't trust JS equality *)
    | Attribute a, Attribute b -> a = b
    | Attribute _, Property _
    | Property _, Attribute _
      -> false

  let attribute name value = AttrKey.Attribute_name name, Attribute value
  let property  name value = AttrKey.Property_name name, Property value

  let class_ c = attribute "class" c
  let classes classes = class_ (String.concat " " classes)

  let id s = attribute "id" s

  let string_property name value =
    AttrKey.Property_name name, Property (Js.Unsafe.inject (Js.string value))

  let on event (handler : ('b #Dom.event as 'a) Js.t -> unit) : pair =
    let f e = handler e; Js._true in
    property ("on" ^ event) (Js.Unsafe.inject (Dom.handler f))

  let style props =
    let obj = Js.Unsafe.obj [||] in
    List.iter (fun (k, v) ->
      Js.Unsafe.set obj (Js.string k) (Js.string v))
      props;
    property "style" obj

  let style_css css =
    attribute "style" css

  let on_focus = on "focus"
  let on_blur  = on "blur"

  let on_click      = on "click"
  let on_mousemove  = on "mousemove"
  let on_mouseup    = on "mouseup"
  let on_mousedown  = on "mousedown"
  let on_mouseenter = on "mouseenter"
  let on_mouseleave = on "mouseleave"
  let on_mouseover  = on "mouseover"
  let on_mouseout   = on "mouseout"

  let on_keyup    = on "keyup"
  let on_keypress = on "keypress"
  let on_keydown  = on "keydown"

  let on_input_event event handler =
    on event (fun ev ->
      Js.Opt.iter (ev##.target) (fun target ->
        Js.Opt.iter (Dom_html.CoerceTo.input target) (fun input ->
          let text = Js.to_string (input##.value) in
          handler input text
        )))

  let on_change = on_input_event "change"
  let on_input  = on_input_event "input"

  let canonicalize_pair : pair -> (string * value) = function
    | AttrKey.Property_name name, (Property _ as value) -> (name, value)
    | AttrKey.Attribute_name name, (Attribute _ as value) -> (name, value)
    | AttrKey.Property_name _, Attribute _
    | AttrKey.Attribute_name _, Property _
      -> assert false

  let list_to_obj attrs : < > Js.t =
    let attrs_obj = Js.Unsafe.obj [||] in
    let open AttrKey in
    List.iter (function
      | Property_name name, Property (value) ->
        Js.Unsafe.set attrs_obj
          (Js.string name)
          value
      | Attribute_name name, Attribute (value) ->
        if not (Js.Optdef.test attrs_obj##.attributes)
        then attrs_obj##.attributes := Js.Unsafe.obj [||];
        Js.Unsafe.set (attrs_obj##.attributes)
          (Js.string name)
          value
      | Property_name _, Attribute _
      | Attribute_name _, Property _
        -> assert false
      )
      attrs;
    attrs_obj

  let list_to_attrs (attrs: pair list) : value AttrMap.t =
    List.fold_left (fun attrs (name, prop) ->
      AttrMap.add name prop attrs
    ) AttrMap.empty attrs
end

module Pure = struct
  (* pure VDOM widgets, which have no knowledge of HTML / JS
   * but do have the concept of internal + user-assigned identities *)
  type internal_tag = int
  type user_tag = [`String of string | `Int of int ]
  type identity = | User_tag of user_tag | Internal_tag of internal_tag

  module Identity = struct
    type t = identity
    let compare = Pervasives.compare
    let eq : t -> t -> bool = (==)
  end

  module IdentityMap = Map.Make(Identity)

  type element = 
    {
      e_name : string;
      e_attrs: Attr.value AttrMap.t;
      e_children: node list;
    }

  and text_node = string

  and raw_node =
    | Element of element
    | Text of string

  and node =
    | Anonymous of raw_node
    (* XXX actually make use of these! *)
    | Identified of (identity * raw_node)

  let string_of_element { e_attrs; e_children; e_name } =
    let open Attr in
    let attrs = e_attrs |> AttrMap.bindings |> List.map (function
      | AttrKey.Property_name key, _ -> "[." ^ key ^ "]"
      | AttrKey.Attribute_name key, Attribute value -> key ^ "=\"" ^ value ^ "\""
      | _ -> failwith "impossible!"
    ) |> String.concat " " in
    Printf.sprintf "<%s %s (%d children)>" e_name attrs (List.length e_children)

  let string_of_raw = function
    | Element e -> string_of_element e
    | Text t -> "<#text: " ^ t ^ ">"

  let string_of_node = function
    | Anonymous raw -> string_of_raw raw
    | Identified (_id, raw) -> string_of_raw raw

  let identify id = function
    | Anonymous node | Identified (_, node) -> Identified (id, node)

  let identify_anonymous id = function
    | Anonymous node -> Identified (id, node)
    | Identified _ as node -> node
end

let identify user_id = let open Pure in identify (User_tag user_id)

type node = Pure.node

(* Assumes HTML *)
exception Assertion_error of string

module Diff = struct
  open Pure (* going away? *)
  type vdom = Pure.node
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
      | Before next ->
        (* Printf.eprintf "Inserting element before nextSibling ...\n"; *)
        (* let _parent = next##.parentNode |> force_opt in *)
        (* if _parent <> (parent:>Dom.node Js.t) then failwith "Parent node mismatch!"; *)
        Js.some next
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

  let set_attr (element:element) (key:AttrKey.t) (value:Attr.value) : unit =
    let open Attr in
    match canonicalize_pair (key, value) with
      | key, Attribute value ->
          element##(setAttribute (Js.string key) (Js.string value))
      | key, Property value -> Js.Unsafe.set element (Js.string key) value

  let remove_attr (element:element) (key:AttrKey.t) : unit =
    let open AttrKey in
    match key with
      | Attribute_name key -> element##removeAttribute (Js.string key)
      | Property_name key -> Js.Unsafe.delete element (Js.string key)


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
    Printf.eprintf "replacing contents\n";
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
        old_attrs := AttrMap.remove key !old_attrs;
        false
      ) else true
    ) in

    (* any old_values that aren't identical in the new view *)
    !old_attrs |> AttrMap.iter (fun key _ ->
      remove_attr element key
    );

    (* Once all the old stuff is gone, add the new values. We need to do
     * this last in case e.g. an attribute switches to a property, in which
     * case unsetting the old value might clobber the new one *)
    new_values |> AttrMap.iter (set_attr element)

  let invalid_dom () =
    raise (Assertion_error "Invalid DOM state!")

  let nth_child element idx : any_node option =
    element##.childNodes##item(idx) |> Js.Opt.to_option

  let rec update_children previous current (`Target_element (parent, _)) : unit = (
    let previous_remaining = ref previous in
    let force_dom_node idx : any_node = nth_child parent idx |> force_option in

    Printf.eprintf "processing %d children (currently there are %d)\n"
      (List.length current)
      (List.length previous);

    current |> List.iteri (fun idx current_child ->
      Printf.eprintf "processing node %s at idx %d\n" (string_of_node current_child) idx;
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
                Printf.eprintf "found matching element for %s\n" (string_of_raw current_child);
                Some (previous_child)
              | Anonymous (Text _), Text _ -> Some (previous_child)
              | _ ->
                Printf.eprintf "Existing node is %s, which is not suitable for %s\n"
                  (string_of_node previous_child)
                  (string_of_raw current_child);
                None
            )
          ) in
          match previous_matching_child with
            | None -> (* No match found; just insert it *)
              Printf.eprintf "inserting before existing node at idx %d\n" idx;
              add_child ~parent (Before (force_dom_node idx)) (render current_child)
            | Some previous_matching_child when previous_matching_child = previous_child ->
              (* no reordering required *)
              Printf.eprintf "node %s matched existing node %s\n"
                (string_of_node current_child)
                (string_of_node previous_matching_child);

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
                    Printf.eprintf "removing node %s\n" (string_of_node candidate);
                    remove (`Target_node (force_dom_node idx, parent));
                    remove_leading_nodes tail
                  )
              ) in
              previous_remaining := remove_leading_nodes !previous_remaining;

              Printf.eprintf "updating node %s -> %s\n"
                (string_of_node previous_matching_child)
                (string_of_node current_child);

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
          Printf.eprintf "Removing node at idx %d (for %s)\n" idx (string_of_node vdom_node);
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
    Printf.eprintf "Removing %d trailing nodes after updating %d to %d\n"
      (List.length !previous_remaining)
      (List.length previous)
      (List.length current);
    remove_trailing_nodes !previous_remaining (List.length current);
  )

  and replace_text : text_node -> string -> unit = fun target current ->
    target##.data := (Js.string current)

  and update_element
      ({ e_name = previous_name; e_attrs = previous_attrs; e_children = previous_children } as previous)
      ({ e_name = current_name;  e_attrs = current_attrs;  e_children = current_children  } as current)
      (target:element_target) : unit =
    Printf.eprintf "updade_element %s -> %s\n"
      (string_of_element previous)
      (string_of_element current);
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
    add_child ~parent Append (render state)

  let update (previous:vdom) (current:vdom) (root:element) =
    Printf.eprintf "processing new vdom %s\n" (string_of_node current);
    update_node previous current (only_target_of_parent root)

end

module Node : sig
  type t = Pure.node
  val text : string -> t

  val create
    :  string
    -> Attr.pair list
    -> t list
    -> t

  val svg
    :  string
    -> Attr.pair list
    -> t list
    -> t

end = struct
  open Pure
  type t = node

  let text (s : string) =
    Anonymous (Text s)

  let create tag (attrs : Attr.pair list) children =
    Anonymous (Element {
      e_name = tag;
      e_attrs = Attr.list_to_attrs attrs;
      e_children = children
    })

  let svg tag (attrs : Attr.pair list) children =
    Anonymous (Element {
      e_name = tag;
      e_attrs = Attr.list_to_attrs attrs;
      e_children = children
    })
end

(* Below basically nicked from vdom *)

module type XML =
  Xml_sigs.T
  with type uri = string
   (* TODO: use a proper response instead of bool. *)
    (* and type handler_result = [ *)
    (*   | `Unhandled (* allow default, allow propagation *) *)
    (*   | `Handled (* prevent default, allow propagation *) *)
    (*  *)
    (*   (* less widely used *) *)
    (*   | `Cancel (* prevent default, stop propagation *) *)
    (*   | `Stop_propagation (* allow default, stop propagation *) *)
    (* ] *)
    and type event_handler = Dom_html.event Js.t -> bool
    and type mouse_event_handler = Dom_html.mouseEvent Js.t -> bool
    and type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> bool
    and type elt = Node.t

module Xml = struct

  module W = Xml_wrap.NoWrap
  type 'a wrap = 'a
  type 'a list_wrap = 'a list

  type uri = string
  let uri_of_string s = s
  let string_of_uri s = s
  type aname = string

  class type biggest_event = object
    inherit Dom_html.event
    inherit Dom_html.mouseEvent
    inherit Dom_html.keyboardEvent
  end

  type biggest_event_handler = biggest_event Js.t -> bool
  type event_handler = Dom_html.event Js.t -> bool
  type mouse_event_handler = Dom_html.mouseEvent Js.t -> bool
  type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> bool
  type attrib = Attr.pair

  let attr name value =
    match name with
    | "value" | "checked" | "selected" ->
      Attr.property name (Js.Unsafe.inject (Js.string value))
    | name ->
      Attr.attribute name value

  let attr_ev name value =
    Attr.property name (Js.Unsafe.inject (fun ev -> Js.bool (value ev)))

  let float_attrib name value : attrib = attr name (string_of_float value)
  let int_attrib name value = attr name (string_of_int value)
  let string_attrib name value = attr name value
  let space_sep_attrib name values = attr name (String.concat " " values)
  let comma_sep_attrib name values = attr name (String.concat "," values)
  let event_handler_attrib name (value : event_handler) =
    attr_ev name (value :> (biggest_event_handler))
  let mouse_event_handler_attrib name (value : mouse_event_handler) =
    attr_ev name (value :> (biggest_event_handler))
  let keyboard_event_handler_attrib name (value : keyboard_event_handler) =
    attr_ev name (value :> (biggest_event_handler))
  let uri_attrib name value = attr name value
  let uris_attrib name values = attr name (String.concat " " values)

  (** Element *)

  type elt = Node.t
  type ename = string

  let make_a x = x

  let empty () = assert false
  let comment _c = assert false

  let pcdata s = Node.text s
  let encodedpcdata s = Node.text s
  let entity e =
    let entity = Dom_html.decode_html_entities (Js.string ("&" ^ e ^ ";")) in
    Node.text (Js.to_string entity)

  let leaf ?(a=[]) name =
    Node.create name (make_a a) []
  let node ?(a=[]) name children =
    Node.create name (make_a a) children

  let cdata s = pcdata s
  let cdata_script s = cdata s
  let cdata_style s = cdata s
end

module Xml_Svg = struct
  include Xml

  let leaf ?(a = []) name =
    Node.svg name (make_a a) []

  let node ?(a = []) name children =
    Node.svg name (make_a a) children
end

module Svg = Svg_f.Make(Xml_Svg)
module Html = Html5_f.Make(Xml)(Svg)
