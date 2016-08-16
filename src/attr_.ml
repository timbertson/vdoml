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

module AttrKey = struct
  type t = Property_name of string | Attribute_name of string
  let compare : t -> t -> int = Pervasives.compare
end
module AttrMap = Map.Make(AttrKey)

module Attr = struct
  type key = AttrKey.t
  type property =
    | Event_handler of (biggest_event Js.t -> event_response)
    | String_prop of string

  type value = 
    | Property of property
    | Attribute of string
  type t = key * value
  type optional = t option

  let eq a b = match (a,b) with
    | Property a, Property b -> a = b
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

  let attribute name value = Some (AttrKey.Attribute_name name, Attribute value)
  let property  name value = Some (AttrKey.Property_name name, Property value)

  let event_handler_attrib name value = property name (Event_handler value)

  let js_of_property = function
    | String_prop s -> Js.string s |> Js.Unsafe.inject
    | Event_handler handler ->
      let handler e =
        match handler e with
          | `Unhandled -> ()
          | `Handled -> Dom.preventDefault e
          | `Stop -> Dom.preventDefault e; Dom_html.stopPropagation e;
      in
      Js.Unsafe.inject handler

  let string_property name value =
    Some (AttrKey.Property_name name, Property (String_prop value))

  let canonicalize_pair : t -> (string * value) = function
    | AttrKey.Property_name name, (Property _ as value) -> (name, value)
    | AttrKey.Attribute_name name, (Attribute _ as value) -> (name, value)
    | AttrKey.Property_name _, Attribute _
    | AttrKey.Attribute_name _, Property _
      -> assert false

  let list_to_attrs (attrs: optional list) : value AttrMap.t =
    List.fold_left (fun attrs -> function
      | Some (name, prop) -> AttrMap.add name prop attrs
      | None -> attrs
    ) AttrMap.empty attrs

  (* TODO: support classList rather than a flat class string *)
end

