open Attr_

module Vdom = struct
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

  module Internal : sig
    (* A strictly controlled module signature to ensure that
     * all internal types are opaque, forcing all node
     * traversal to happen via this module, which ensures
     * the type-unsafe "transform" nodes are applied
     *)
    type 'msg pure_node
    type 'msg pure_element
    type 'msg pure_raw_node

    type 'msg internal_element
    type 'msg internal_property
    type 'msg internal_node

    type 'msg attr =
      | Property of 'msg internal_property
      | Attribute of string

    type 'msg element =
      {
        e_name : string;
        e_attrs: 'msg attr AttrMap.t;
        e_children: 'msg internal_node list;
      }

    type 'msg raw_node = 
      | Element of 'msg internal_element
      | Text of string

    type 'msg node = 
      | Anonymous of 'msg raw_node
      | Identified of (identity * 'msg raw_node)

    val transform : ('a -> 'b) -> 'a pure_node -> 'b pure_node
    val root : 'msg pure_node -> 'msg internal_node

    (* private -> raw converters *)
    val get_node : 'msg internal_node -> 'msg node
    val get_element : 'msg internal_element -> 'msg element

    val element_name : 'msg internal_element -> string

    val property_eq : 'msg internal_property -> 'msg internal_property -> bool
    val attr_eq : 'msg attr -> 'msg attr -> bool
    val js_of_property : ('msg -> unit) -> 'msg internal_property -> Js.Unsafe.any
    val string_of_attr : (AttrKey.t * 'msg attr) -> string
    val identify : identity -> 'msg pure_node -> 'msg pure_node
    val identify_anonymous : identity -> 'msg pure_node -> 'msg pure_node
    val identity_of_node : 'msg internal_node -> identity option

  end = struct
    (* pure_:
     * Built up by creation functions.
     * They form a 'msg pure_node, but *must* only be accessed via the
     * functions in this module in order to be type-safe.
     *
     * internal_:
     * Used while walking the tree - allows us to
     * apply the accessor incrementally, rather than processing the entire tree
     * up front. As you access each node, you get:
     *
     * (unprefixed):
     * The only externally-visible type, allowing users to
     * pattern match and traverse, while containing only
     * interal_* components in order to enforce type-safe access.
     *)
    type 'msg accessor = {
      apply: ('msg -> 'msg);
      identity: ('msg -> 'msg) list;
    }

    type 'msg element = 
      {
        e_name : string;
        e_attrs: 'msg attr AttrMap.t;
        e_children: 'msg internal_node list;
      }

    and 'msg pure_element = 
      {
        e_pure_name : string;
        e_pure_attrs: 'msg Attr.value AttrMap.t;
        e_pure_children: 'msg pure_node list;
      }

    and 'msg attr =
      | Property of 'msg internal_property
      | Attribute of string

    and text_node = string

    and 'msg raw_node =
      | Element of 'msg internal_element
      | Text of string

    and 'msg pure_raw_node =
      | Pure_element of 'msg pure_element
      | Pure_text of string

    and 'msg internal_element = 'msg accessor * 'msg pure_element
    and 'msg internal_property = 'msg accessor * 'msg Attr.property
    and 'msg internal_node = 'msg accessor * 'msg pure_node

    and 'msg pure_node =
      | Pure_anonymous of 'msg pure_raw_node
      | Pure_identified of (identity * 'msg pure_raw_node)
      | Pure_transformer of 'msg type_conversion_node

    and 'msg node =
      | Anonymous of 'msg raw_node
      | Identified of (identity * 'msg raw_node)

    and 'msg type_conversion_node =
      { 
        unsafe_convert : 'msg -> 'msg; (* Note: 'a -> 'msg *)
        unsafe_content : 'msg pure_node; (* NOTE: this is _actually_ 'a pure_node *)
      }

    (* Note: for efficient Vdom diffing, `fn` should be statically-defined
     * rather than an anonymous function, as it's used in equality checking
     *)
    let transform : ('a -> 'b) -> 'a pure_node -> 'b pure_node = fun convert pure_node ->
      Pure_transformer {
        unsafe_convert = Obj.magic convert;
        unsafe_content = Obj.magic pure_node;
      }

    let property_eq (a_ctx, a) (b_ctx, b) =
      let { identity = a_id ; apply = _a_apply } = a_ctx in
      let { identity = b_id ; apply = _b_apply } = b_ctx in
      a_id = b_id && a = b

    let attr_eq a b = match (a, b) with
      | Attribute a, Attribute b -> a = b
      | Property a, Property b -> property_eq a b
      | Attribute _, Property _ | Property _, Attribute _ -> false

    let init = {
      apply = (fun x -> x);
      identity = [];
    }

    let root (node: 'msg pure_node) : 'msg internal_node = (init, node)

    let rec get_node (ctx, value) =
      let get_raw_node = function
        | Pure_element e -> Element (ctx, e)
        | Pure_text t -> Text t
      in
      match value with
      | Pure_anonymous raw -> Anonymous (get_raw_node raw)
      | Pure_identified (id, raw) -> Identified (id, get_raw_node raw)
      | Pure_transformer child ->
        let { apply; identity } = ctx in
        (* recurse with our new context *)
        get_node ({
          apply = (fun msg -> apply (child.unsafe_convert msg));
          identity = child.unsafe_convert :: identity;
        }, child.unsafe_content)

    let get_element (ctx, element) = 
      let { e_pure_name; e_pure_attrs; e_pure_children} = element in
      {
        e_name = e_pure_name;
        e_attrs = e_pure_attrs |> AttrMap.map (function
          | Attr.Attribute value -> Attribute value
          | Attr.Property p -> Property (ctx, p)
        );
        e_children = e_pure_children |> List.map (fun child -> (ctx, child));
      }

    let element_name (ctx, { e_pure_name; _}) = e_pure_name

    let string_of_attr (name, attr) = match attr with
      | Attribute value -> Attr.string_of_attr (name, Attr.Attribute value)
      | Property _ -> Attr.string_of_attr_name name

    let js_of_property (emit:'msg -> unit) ((ctx, prop):'msg internal_property) =
      let open Attr in
      Attr.js_of_property ~emit (match prop with
        | String_prop x -> String_prop x
        | Message_emitter (msg, response) -> Message_emitter (ctx.apply msg, response)
        | Message_fn fn -> Message_fn (fun event -> match fn event with
          | Some msg -> Some (ctx.apply msg)
          | None -> None
        )
        | Message_response_fn fn -> Message_response_fn (fun event -> match fn event with
          | Some (msg, response) -> Some ((ctx.apply msg), response)
          | None -> None
        )
      )

    let rec identify id = function
      | Pure_anonymous node | Pure_identified (_, node) -> Pure_identified (id, node)
      | Pure_transformer node -> Pure_transformer { node with
        unsafe_content = identify id node.unsafe_content
      }

    let rec identify_anonymous id = function
      | Pure_anonymous node -> Pure_identified (id, node)
      | Pure_identified _ as node -> node
      | Pure_transformer node -> Pure_transformer { node with
        unsafe_content = identify_anonymous id node.unsafe_content
      }

    let rec identity_of_node (_ctx, node) = match node with
      | Pure_anonymous _ -> None
      | Pure_identified (id, _) -> Some id
      | Pure_transformer node -> identity_of_node (_ctx, node.unsafe_content)
  end

  type 'msg html = 'msg Internal.pure_node
    include Internal
  (* TODO: *)
  let string_of_element _ = failwith "TODO"
  let string_of_raw _ = failwith "TODO"
  let string_of_node _ = failwith "TODO"
  let string_of_attr _ = failwith "TODO"

  (* open Internal *)
  (*  *)
  (* let string_of_element { e_attrs; e_children; e_name } = *)
  (*   let attrs = e_attrs |> AttrMap.bindings |> List.map (string_of_attr) |> String.concat " " in *)
  (*   Printf.sprintf "<%s %s (%d children)>" e_name attrs (List.length e_children) *)
  (*  *)
  (* let string_of_raw = function *)
  (*   | Element e -> string_of_element e *)
  (*   | Text t -> "<#text: " ^ t ^ ">" *)
  (*  *)
  (* let string_of_node = function *)
  (*   | Anonymous raw -> string_of_raw raw *)
  (*   | Identified (_id, raw) -> string_of_raw raw *)

end
