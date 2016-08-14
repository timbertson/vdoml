type vdom_node = Vdom_.Vdom.node

type event_response = [
  | `Unhandled
  | `Handled
  | `Stop
]

type event_handler = Dom_html.event Js.t -> event_response
type mouse_event_handler = Dom_html.mouseEvent Js.t -> event_response
type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> event_response

include Html5_sigs.T with
  type +'elt elt = vdom_node
  and type +'a attrib = Attr_.Attr.pair
  and type 'a Xml.W.t = 'a
  and type 'a Xml.W.tlist = 'a list
  and type Xml.event_handler = event_handler
  and type Xml.mouse_event_handler = mouse_event_handler
  and type Xml.keyboard_event_handler = keyboard_event_handler
  and type Xml.uri = string
