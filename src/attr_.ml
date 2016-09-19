open Util_

module AttrKey = struct
  type t = Property_name of string | Attribute_name of string
  let compare : t -> t -> int = Pervasives.compare
end
module AttrMap = Map.Make(AttrKey)

module Attr = struct
  type key = AttrKey.t

  type 'msg property =
    | String_prop of string
    | Message_emitter of 'msg * Event.response
    | Event_handler of (Dom_html.event Js.t -> 'msg Event.result)

  type 'msg value =
    | Property of 'msg property
    | Attribute of string

  type 'msg t = key * 'msg value
  type 'msg optional = 'msg t option

  let property_eq a b = match (a,b) with
    | String_prop a, String_prop b -> a = b
    | String_prop _, _ -> false
    | Message_emitter (a1, a2), Message_emitter (b1, b2) -> a1 = b1 && a2 = b2
    | Message_emitter _, _ -> false
    | Event_handler a_fn, Event_handler b_fn -> a_fn == b_fn (* cannot compare functions structurally *)
    | Event_handler _, _ -> false

  let eq a b = match (a,b) with
    | Property a, Property b -> property_eq a b
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
  let string_property name value = property name (String_prop value)

  let message_emitter ?(response=`Handled) value = Message_emitter (value, response)
  let event_handler fn = Event_handler fn

  let js_of_property ~emit =
    function
      | String_prop s -> Js.string s |> Js.Unsafe.inject
      | Message_emitter (msg, response) -> Js.Unsafe.inject (fun _e ->
          emit msg;
          response
      )
      | Event_handler handler -> Js.Unsafe.inject (fun e ->
        let result = handler e in
        Event_.Event.apply emit (result:>'msg Event.result)
    )


  let canonicalize_pair : 'msg t -> (string * 'msg value) = function
    | AttrKey.Property_name name, (Property _ as value) -> (name, value)
    | AttrKey.Attribute_name name, (Attribute _ as value) -> (name, value)
    | AttrKey.Property_name _, Attribute _
    | AttrKey.Attribute_name _, Property _
      -> assert false

  let list_to_attrs (attrs: 'msg optional list) : 'msg value AttrMap.t =
    List.fold_left (fun attrs pair ->
      match pair with
      | Some (name, prop) -> AttrMap.add name prop attrs
      | None -> attrs
    ) AttrMap.empty attrs

  (* TODO: support classList rather than a flat class string *)
end

