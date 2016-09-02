type response = Event_types_.response
class type biggest_event = Event_types_.biggest_event
type 'msg result = 'msg Event_types_.result

val response : 'msg result -> response
val message : 'msg result -> 'msg option

val return : response -> 'msg result

val emit : ?response:response -> 'msg -> 'msg result
val optional : 'msg result option -> 'msg result

val handled : 'msg result
val unhandled : 'msg result
val stop : 'msg result

(* Utilities for processing events *)
val mouse_event : #Dom_html.event Js.t -> Dom_html.mouseEvent Js.t option
val keyboard_event : #Dom_html.event Js.t -> Dom_html.keyboardEvent Js.t option
val target : #Dom_html.event Js.t -> Dom_html.element Js.t option

val coerce : ('a -> 'b Js.Opt.t) -> 'a -> 'b option
val coerce_target : (Dom_html.element Js.t -> 'a Js.t Js.opt) -> biggest_event Js.t -> 'a Js.t option

val input_contents : biggest_event Js.t -> string option
