open Attr_
open Event_

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
    type 'msg internal_property

    type 'msg attr =
      | Property of 'msg internal_property
      | Attribute of string

    type hook = Dom_html.element Js.t -> unit
    type hooks = {
      hook_create: hook option;
      hook_destroy: hook option;
    }

    type 'msg element = {
      e_name : string;
      e_attrs: 'msg attr AttrMap.t;
      e_children: 'msg node list;
      e_hooks: hooks;
    }

    and 'msg raw_node = 
      | Element of 'msg element
      | Text of string

    and 'msg node = 
      | Anonymous of 'msg raw_node
      | Identified of (identity * 'msg raw_node)

    val transform : ('a -> 'b) -> 'a pure_node -> 'b pure_node
    val root : 'msg pure_node -> 'msg node

    val property_eq : 'msg internal_property -> 'msg internal_property -> bool
    val attr_eq : 'msg attr -> 'msg attr -> bool
    val js_of_property : ('msg -> unit) -> 'msg internal_property -> Js.Unsafe.any
    val string_of_attr : (AttrKey.t * 'msg attr) -> string
    val identify : identity -> 'msg pure_node -> 'msg pure_node
    val identify_anonymous : identity -> 'msg pure_node -> 'msg pure_node
    val hook : hooks -> 'msg pure_node -> 'msg pure_node
    val hooks : ?create:hook -> ?destroy:hook -> unit -> hooks
    val text : string -> 'msg pure_node
    val create : ?a:'msg Attr.optional list -> string -> 'msg pure_node list -> 'msg pure_node
    val create_leaf : ?a:'msg Attr.optional list -> string -> 'msg pure_node
  end = struct
    (* pure_:
     * Built up by creation functions.
     * They form a 'msg pure_node, but *must* only be accessed via the
     * functions in this module in order to be type-safe.
     *
     * (unprefixed):
     * The only externally-visible type, only given out by
     * `root` after it has traversed the tree and applied all
     * transforms
     *)
    type 'msg accessor = {
      apply: ('msg -> 'msg);
      identity: ('msg -> 'msg) list;
    }

    type hook = Dom_html.element Js.t -> unit
    type hooks = {
      hook_create: hook option;
      hook_destroy: hook option;
    }

    type 'msg element = 
      {
        e_name : string;
        e_attrs: 'msg attr AttrMap.t;
        e_children: 'msg node list;
        e_hooks: hooks;
      }

    and 'msg pure_element =
      {
        e_pure_name : string;
        e_pure_attrs: 'msg Attr.value AttrMap.t;
        e_pure_children: 'msg pure_node list;
        e_pure_hooks: hooks;
      }

    and 'msg attr =
      | Property of 'msg internal_property
      | Attribute of string

    and text_node = string

    and 'msg raw_node =
      | Element of 'msg element
      | Text of string

    and 'msg pure_raw_node =
      | Pure_element of 'msg pure_element
      | Pure_text of string

    and 'msg internal_property = 'msg accessor * 'msg Attr.property

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
        unsafe_content : 'msg pure_node; (* NOTE: this is actually 'a pure_node *)
      }

    (* Note: for efficient Vdom diffing, `fn` should be statically-defined
     * rather than an anonymous function, as it's used in equality checking
     *)
    let transform : ('a -> 'b) -> 'a pure_node -> 'b pure_node = fun convert pure_node ->
      Pure_transformer {
        unsafe_convert = Obj.magic convert;
        unsafe_content = Obj.magic pure_node;
      }

    let identity_eq =
      (* Identity is a list of functions, which
       * can only be compared for physical equality *)
      let rec eq a b =
        match a, b with
          | [], [] -> true
          | a::aa, b::bb -> a == b && eq aa bb
          | [], _ | _, [] -> false
      in
      eq

    let property_eq (a_ctx, a) (b_ctx, b) =
      let { identity = a_id ; apply = _a_apply } = a_ctx in
      let { identity = b_id ; apply = _b_apply } = b_ctx in
      identity_eq a_id b_id && Attr.property_eq a b

    let attr_eq a b = match (a, b) with
      | Attribute a, Attribute b -> a = b
      | Property a, Property b -> property_eq a b
      | Attribute _, Property _ | Property _, Attribute _ -> false

    let rec convert_node ctx value =
      match value with
      | Pure_anonymous raw -> Anonymous (convert_raw ctx raw)
      | Pure_identified (id, raw) -> Identified (id, convert_raw ctx raw)
      | Pure_transformer child ->
        let { apply; identity } = ctx in
        (* recurse with our new context *)
        let ctx = {
          apply = (fun msg -> apply (child.unsafe_convert msg));
          identity = child.unsafe_convert :: identity;
        } in
        convert_node ctx child.unsafe_content

    and convert_raw ctx = function
      | Pure_element e -> Element (convert_element ctx e)
      | Pure_text t -> Text t

    and convert_element ctx element =
      let { e_pure_name; e_pure_attrs; e_pure_children; e_pure_hooks} = element in
      {
        e_name = e_pure_name;
        e_hooks = e_pure_hooks;
        e_attrs = e_pure_attrs |> AttrMap.map (function
          | Attr.Attribute value -> Attribute value
          | Attr.Property p -> Property (ctx, p)
        );
        e_children = e_pure_children |> List.map (convert_node ctx);
      }

    let init = {
      apply = (fun x -> x);
      identity = [];
    }

    let root (node: 'msg pure_node) : 'msg node =
      convert_node init node

    let element_name (ctx, { e_pure_name; _}) = e_pure_name

    let string_of_attr (name, attr) = match attr with
      | Attribute value -> Attr.string_of_attr (name, Attr.Attribute value)
      | Property _ -> Attr.string_of_attr_name name

    let js_of_property (emit:'msg -> unit) ((ctx, prop):'msg internal_property) =
      let open Attr in
      Attr.js_of_property ~emit (match prop with
        | String_prop x -> String_prop x
        | Event_handler handler -> Event_handler (fun e -> Event.map_msg ctx.apply (handler e))
        | Message_emitter (msg, response) -> Message_emitter (ctx.apply msg, response)
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

    let hooks ?create ?destroy () = {
      hook_create = create;
      hook_destroy = destroy;
    }

    let rec hook_node hooks = function
      | Pure_text _ -> failwith "Only element nodes can be hooked"
      | Pure_element e -> Pure_element { e with e_pure_hooks = hooks }

    let rec hook hooks = function
      | Pure_anonymous node -> Pure_anonymous (hook_node hooks node)
      | Pure_identified (id, node) -> Pure_identified (id, hook_node hooks node)
      | Pure_transformer node -> Pure_transformer { node with
        unsafe_content = hook hooks node.unsafe_content
      }

    let text (s : string) =
      Pure_anonymous (Pure_text s)

    let no_hooks = {
      hook_create = None;
      hook_destroy = None;
    }

    let create ?(a=[]) tag children =
      Pure_anonymous (Pure_element {
        e_pure_name = tag;
        e_pure_hooks = no_hooks;
        e_pure_attrs = Attr.list_to_attrs a;
        e_pure_children = children
      })

    let create_leaf ?a tag =
      create tag ?a []
  end

  type 'msg html = 'msg Internal.pure_node
  include Internal
end
