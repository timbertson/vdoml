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
  type 'msg property =
    | String_prop of string
    | Message_emitter of 'msg * event_response
    | Message_fn of (biggest_event Js.t -> 'msg option)
    | Message_response_fn of (biggest_event Js.t -> ('msg * event_response) option)

  type 'msg value =
    | Property of 'msg property
    | Attribute of string

  type 'msg t = key * 'msg value
  type 'msg optional = 'msg t option

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

  let message_emitter_attr ?(response=`Handled) name value = property name (Message_emitter (value, response))
  let message_fn_attr name value = property name (Message_fn value)
  let message_response_fn_attr name value = property name (Message_response_fn value)

  let js_of_property ~emit =
    let wrap handler = Js.Unsafe.inject (fun e ->
      match handler e with
        | `Unhandled -> ()
        | `Handled -> Dom.preventDefault e
        | `Stop -> Dom.preventDefault e; Dom_html.stopPropagation e;
    ) in
    function
      | String_prop s -> Js.string s |> Js.Unsafe.inject
      | Message_emitter (msg, response) -> wrap (fun _evt -> emit msg; response)
      | Message_fn fn -> wrap (fun e ->
        match fn e with
          | Some msg -> emit msg; `Handled
          | None -> `Unhandled
      )
      | Message_response_fn fn -> wrap (fun e ->
        match fn e with
          | Some (msg, response) -> emit msg; response
          | None -> `Unhandled
      )

  let string_property name value =
    Some (AttrKey.Property_name name, Property (String_prop value))

  let canonicalize_pair : 'msg t -> (string * 'msg value) = function
    | AttrKey.Property_name name, (Property _ as value) -> (name, value)
    | AttrKey.Attribute_name name, (Attribute _ as value) -> (name, value)
    | AttrKey.Property_name _, Attribute _
    | AttrKey.Attribute_name _, Property _
      -> assert false

  let list_to_attrs (attrs: 'msg optional list) : 'msg value AttrMap.t =
    List.fold_left (fun attrs -> function
      | Some (name, prop) -> AttrMap.add name prop attrs
      | None -> attrs
    ) AttrMap.empty attrs

  (* TODO: support classList rather than a flat class string *)
end

