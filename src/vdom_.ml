open Attr_

module type Vdom_s = sig
  type message
  type internal_tag = int
  type user_tag = [`String of string | `Int of int ]
  type identity = | User_tag of user_tag | Internal_tag of internal_tag
  type element = 
    {
      e_name : string;
      e_attrs: message Attr.value AttrMap.t;
      e_children: node list;
    }

  and text_node = string

  and raw_node =
    | Element of element
    | Text of string

  and node =
    | Anonymous of raw_node
    | Identified of (identity * raw_node)
end

module Make(App:App_.S) = struct
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
      e_attrs: App.message Attr.value AttrMap.t;
      e_children: node list;
    }

  and text_node = string

  and raw_node =
    | Element of element
    | Text of string

  and node =
    | Anonymous of raw_node
    | Identified of (identity * raw_node)

  let string_of_element { e_attrs; e_children; e_name } =
    let attrs = e_attrs |> AttrMap.bindings |> List.map (Attr.string_of_attr) |> String.concat " " in
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
