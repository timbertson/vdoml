open Js_of_ocaml
open Attr_
open Event_
open Util_
open Event_chain_

module Vdom = struct
  (* pure VDOM widgets, which have no knowledge of HTML / JS
   * but do have the concept of internal + user-assigned identities *)
  type internal_tag = int
  type user_tag = [`String of string | `Int of int ]
  type identity = | User_tag of user_tag | Internal_tag of internal_tag

  module Identity = struct
    type t = identity
    let compare = Pervasives.compare
    let eq : t -> t -> bool = (=)
    let to_string = function
      | User_tag tag -> "user:" ^ (match tag with
        | `String s -> s
        | `Int i -> string_of_int i
      )
      | Internal_tag i -> "internal:" ^ string_of_int i
  end

  module IdentityMap = Map.Make(Identity)

  module Internal = struct
    type hook = Dom_html.element Js.t -> unit
    type hooks = {
      hook_create: hook option;
      hook_destroy: hook option;
    }

    and 'msg element =
      {
        e_name : string;
        e_attrs: 'msg Attr.map;
        e_children: 'msg node list;
        e_hooks: hooks;
      }

    and 'msg content_node =
      | Element of 'msg element
      | Text of string

    (* A node is any chain of (Conversion, Observer, Identity) modifiers terminating
     * in a Content. A Conversion node is existentially qualified. the mapping from
     * child_msg -> msg is hidden and the overall type is just `msg node *)
    and 'msg node =
      | Conversion: (('child_msg -> 'msg) * 'child_msg node) -> 'msg node
      | Observer: ('msg -> unit) * 'msg node -> 'msg node
      | Identity: identity * 'msg node -> 'msg node
      | Content: 'msg content_node -> 'msg node

    let node_of_element el = Content (Element el)

    let rec element_of_content_node = function
      | Element el -> Some el
      | Text _ -> None

    let cast_mapped_node : type msg msg_a msg_b.
      msg Event_chain.t ->
      ((msg_a -> msg) * msg_a node) ->
      ((msg_b -> msg) * msg_b node) ->
      (msg_b Event_chain.t * msg_b node * msg_b node) option
    = fun ctx (mapa, a) (mapb, b) ->
        if Event_chain.mapper_eq mapa mapb then
          (* proof: if mapa == mapb then the type paramater of `a` and `b` must also equal *)
          Some (Event_chain.add_conversion mapb ctx, Obj.magic a, b)
        else None

    let rec identify_anonymous : type msg. identity -> msg node -> msg node = fun id node -> match node with
      | Conversion (fn, node) -> Conversion (fn, identify_anonymous id node)
      | Observer (fn, node) -> Observer (fn, identify_anonymous id node)
      | Identity _ -> node
      | Content _ -> Identity (id, node)

    let rec identify id node = Identity (id, node)

    let hooks ?create ?destroy () = {
      hook_create = create;
      hook_destroy = destroy;
    }

    let rec hook : type msg. hooks -> msg node -> msg node = fun hooks -> function
      | Conversion (fn, node) -> Conversion (fn, hook hooks node)
      | Observer (fn, node) -> Observer (fn, hook hooks node)
      | Identity (id, node) -> Identity (id, hook hooks node)
      | Content c -> Content (match c with
        | Text _ -> failwith "Only element nodes can be hooked"
        | Element e -> Element { e with e_hooks = hooks }
      )

    let text (s : string) = Content (Text s)

    let observe : ('msg -> unit) -> 'msg node -> 'msg node = fun observe node ->
      Observer (observe, node)

    let no_hooks = {
      hook_create = None;
      hook_destroy = None;
    }

    let create ?(a=[]) tag children =
      Content (Element {
        e_name = tag;
        e_hooks = no_hooks;
        e_attrs = Attr.list_to_attrs a;
        e_children = children
      })

    let create_leaf ?a tag =
      create tag ?a []
  end

  type 'msg html = 'msg Internal.node
  include Internal
end
