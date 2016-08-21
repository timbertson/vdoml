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
  type 'm property =
    | Event_handler of (biggest_event Js.t -> event_response)
    | Message_fn of (biggest_event Js.t -> 'm option)
    | Message of 'm
    | String_prop of string

  type 'm value = 
    | Property of 'm property
    | Attribute of string
  type 'm t = key * 'm value
  type 'm optional = 'm t option

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

  let js_of_property ~emit =
    let wrap fn =
      Js.Unsafe.inject (fun e -> match fn e with
        | `Unhandled -> ()
        | `Handled -> Dom.preventDefault e
        | `Stop -> Dom.preventDefault e; Dom_html.stopPropagation e;
      )
    in
    function
      | String_prop s -> Js.string s |> Js.Unsafe.inject
      | Message m -> wrap (fun _ -> emit m; `Handled)
      | Message_fn fn -> wrap (fun e ->
          match fn e with
            | Some m -> emit m; `Handled
            | None -> `Unhandled
          )
      | Event_handler handler -> wrap handler

  let string_property name value =
    Some (AttrKey.Property_name name, Property (String_prop value))

  let canonicalize_pair : 'm t -> (string * 'm value) = function
    | AttrKey.Property_name name, (Property _ as value) -> (name, value)
    | AttrKey.Attribute_name name, (Attribute _ as value) -> (name, value)
    | AttrKey.Property_name _, Attribute _
    | AttrKey.Attribute_name _, Property _
      -> assert false

  let list_to_attrs (attrs: 'm optional list) : 'm value AttrMap.t =
    List.fold_left (fun attrs -> function
      | Some (name, prop) -> AttrMap.add name prop attrs
      | None -> attrs
    ) AttrMap.empty attrs

  (* TODO: support classList rather than a flat class string *)
end

