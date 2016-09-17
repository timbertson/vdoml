type response = Event_types_.response
type 'msg result = 'msg Event_types_.result

val get_response : 'msg result -> response
val get_message : 'msg result -> 'msg option

(* constructors *)
val respond : response -> 'msg result
val handle : 'msg -> 'msg result
val return : response -> 'msg -> 'msg result

(* preconstructed messge-less responses *)
val handled : 'msg result
val unhandled : 'msg result
val stop : 'msg result

(* Utilities for processing events *)
val optional : 'msg result option -> 'msg result
val mouse_event : #Dom_html.event Js.t -> Dom_html.mouseEvent Js.t option
val keyboard_event : #Dom_html.event Js.t -> Dom_html.keyboardEvent Js.t option
val target : #Dom_html.event Js.t -> Dom_html.element Js.t option

val coerce : ('a -> 'b Js.Opt.t) -> 'a -> 'b option
val coerce_target : (Dom_html.element Js.t -> 'a Js.t Js.opt) -> #Dom_html.event Js.t -> 'a Js.t option

val input_contents : #Dom_html.event Js.t -> string option
