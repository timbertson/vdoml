type event_response = [
  | `Unhandled
  | `Handled
  | `Stop
]

class type biggest_event = object
  inherit Dom_html.event
  inherit Dom_html.mouseEvent
  inherit Dom_html.keyboardEvent
end

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
      -> (Dom_html.event Js.t -> event_response)
      -> pair

    (* TODO: promote the necessary values below to the Html module! *)

    val class_    : string -> pair
    val classes   : string list -> pair
    val id        : string -> pair
    val style     : (string * string) list -> pair
    val style_css : string -> pair

    val on_focus  : (Dom_html.event Js.t -> event_response) -> pair
    val on_blur   : (Dom_html.event Js.t -> event_response) -> pair

    (** [on_input] fires every time the input changes, e.g., whenever a key is pressed in
        the input field.  The current contents are returned as an OCaml string as a
        convenience *)
    val on_input  : (Dom_html.inputElement Js.t -> string -> event_response) -> pair

    (** [on_change] fires when the input is complete, e.g., when enter is pressed in the
        input field.  The current contents are returned as an OCaml string as a
        convenience *)
    val on_change : (Dom_html.inputElement Js.t -> string -> event_response) -> pair

    val on_click      : (Dom_html.mouseEvent Js.t -> event_response) -> pair
    val on_mousemove  : (Dom_html.mouseEvent Js.t -> event_response) -> pair
    val on_mouseup    : (Dom_html.mouseEvent Js.t -> event_response) -> pair
    val on_mousedown  : (Dom_html.mouseEvent Js.t -> event_response) -> pair
    val on_mouseenter : (Dom_html.mouseEvent Js.t -> event_response) -> pair
    val on_mouseleave : (Dom_html.mouseEvent Js.t -> event_response) -> pair
    val on_mouseover  : (Dom_html.mouseEvent Js.t -> event_response) -> pair
    val on_mouseout   : (Dom_html.mouseEvent Js.t -> event_response) -> pair

    val on_keyup    : (Dom_html.keyboardEvent Js.t -> event_response) -> pair
    val on_keypress : (Dom_html.keyboardEvent Js.t -> event_response) -> pair
    val on_keydown  : (Dom_html.keyboardEvent Js.t -> event_response) -> pair
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
    val list_to_attrs : pair list -> value AttrMap.t
    val eq : value -> value -> bool
    val canonicalize_pair : pair -> (string * value)
    val string_of_attr_name : key -> string
    val string_of_attr : pair -> string
    val event_handler_attrib : string -> (biggest_event Js.t -> event_response) -> pair
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

  let string_of_attr_name = function
    | AttrKey.Attribute_name key -> "\"" ^ key ^ "\""
    | AttrKey.Property_name key -> "[."^key^"]"

  let string_of_attr = function
    | AttrKey.Property_name _ as key, _ -> string_of_attr_name key
    | AttrKey.Attribute_name key, Attribute value -> key ^ "=\"" ^ value ^ "\""
    | _ -> failwith "impossible!"

  let attribute name value = AttrKey.Attribute_name name, Attribute value
  let property  name value = AttrKey.Property_name name, Property value

  let event_handler_attrib name value =
    let handler e =
      match value e with
        | `Unhandled -> ()
        | `Handled -> Dom.preventDefault e
        | `Stop -> Dom.preventDefault e; Dom_html.stopPropagation e;
    in
    property name (Js.Unsafe.inject handler)

  let class_ c = attribute "class" c
  let classes classes = class_ (String.concat " " classes)

  let id s = attribute "id" s

  let string_property name value =
    AttrKey.Property_name name, Property (Js.Unsafe.inject (Js.string value))

  let on event (handler : ('b #Dom.event as 'a) Js.t -> event_response) : pair =
    event_handler_attrib ("on" ^ event) handler

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
      let rv = ref None in
      Js.Opt.iter (ev##.target) (fun target ->
        Js.Opt.iter (Dom_html.CoerceTo.input target) (fun input ->
          let text = Js.to_string (input##.value) in
          rv := Some (handler input text)
        )
      );
      match !rv with
        | Some rv -> rv
        | None -> `Unhandled
    )

  let on_change = on_input_event "change"
  let on_input  = on_input_event "input"

  let canonicalize_pair : pair -> (string * value) = function
    | AttrKey.Property_name name, (Property _ as value) -> (name, value)
    | AttrKey.Attribute_name name, (Attribute _ as value) -> (name, value)
    | AttrKey.Property_name _, Attribute _
    | AttrKey.Attribute_name _, Property _
      -> assert false

  let list_to_attrs (attrs: pair list) : value AttrMap.t =
    List.fold_left (fun attrs (name, prop) ->
      AttrMap.add name prop attrs
    ) AttrMap.empty attrs
end

