open Vdom_
open Attr_

type vdom_node = Vdom.node
type vdom_attr = Attr.optional
type event_response = Attr_.event_response
type event_handler = Dom_html.event Js.t -> event_response
type mouse_event_handler = Dom_html.mouseEvent Js.t -> event_response
type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> event_response

module Tyxml_impl = struct
  module Node : sig
    type t = Vdom.node
    val text : string -> t

    val create
      : string
      -> Attr.optional list
      -> t list
      -> t

    val svg
      :  string
      -> Attr.optional list
      -> t list
      -> t

  end = struct
    open Vdom
    type t = node

    let text (s : string) =
      Anonymous (Text s)

    let create tag (attrs : Attr.optional list) children =
      Anonymous (Element {
        e_name = tag;
        e_attrs = Attr.list_to_attrs attrs;
        e_children = children
      })

    let svg tag (attrs : Attr.optional list) children =
      Anonymous (Element {
        e_name = tag;
        e_attrs = Attr.list_to_attrs attrs;
        e_children = children
      })
  end

  (* Below basically nicked from vdom *)

  module type XML = sig
    include Xml_sigs.T with
      type uri = string
      and type event_handler = event_handler
      and type mouse_event_handler = mouse_event_handler
      and type keyboard_event_handler = keyboard_event_handler
      and type elt = Node.t
  end

  module Xml = struct
    module W = Xml_wrap.NoWrap
    type 'a wrap = 'a
    type 'a list_wrap = 'a list

    type uri = string
    let uri_of_string s = s
    let string_of_uri s = s
    type aname = string

    type biggest_event_handler = biggest_event Js.t -> event_response
    type event_handler = Dom_html.event Js.t -> event_response
    type mouse_event_handler = Dom_html.mouseEvent Js.t -> event_response
    type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> event_response
    type attrib = Attr.optional

    let attr name value =
      match name with
      | "value" | "checked" | "selected" ->
        Attr.string_property name value
      | name ->
        Attr.attribute name value

    let float_attrib name value : attrib = attr name (string_of_float value)
    let int_attrib name value = attr name (string_of_int value)
    let string_attrib name value = attr name value
    let space_sep_attrib name values = attr name (String.concat " " values)
    let comma_sep_attrib name values = attr name (String.concat "," values)
    let event_handler_attrib name (value : event_handler) =
      Attr.event_handler_attrib name (value :> (biggest_event_handler))
    let mouse_event_handler_attrib name (value : mouse_event_handler) =
      Attr.event_handler_attrib name (value :> (biggest_event_handler))
    let keyboard_event_handler_attrib name (value : keyboard_event_handler) =
      Attr.event_handler_attrib name (value :> (biggest_event_handler))
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
end

module Svg = Svg_f.Make(Tyxml_impl.Xml_Svg)
module Html = Html5_f.Make(Tyxml_impl.Xml)(Svg)

(* additional html utils not provided by tyxml *)
let a_on event (handler : (Dom_html.event) Js.t -> event_response) : Attr.optional =
  Attr.event_handler_attrib ("on" ^ event) (handler:>(biggest_event Js.t -> event_response))

let s_class c = Attr.attribute "class" c
let text = Html.pcdata

module Input = struct
  type event = {
    event : Dom_html.event Js.t;
    element : Dom_html.inputElement Js.t;
    contents: string;
  }

  let event e = e.event
  let element e = e.element
  let contents e = e.contents

  let lift (handler: event -> event_response) = (fun ev ->
    let ev = (ev :> Dom_html.event Js.t) in
    let rv = ref None in
    Js.Opt.iter (ev##.target) (fun target ->
      Js.Opt.iter (Dom_html.CoerceTo.input target) (fun input ->
        let arg = {
          event = ev;
          element = input;
          contents = Js.to_string (input##.value);
        } in
        rv := Some (handler arg)
      )
    );
    match !rv with
      | Some rv -> rv
      | None -> `Unhandled
  )
end
