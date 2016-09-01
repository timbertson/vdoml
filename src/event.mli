type response = Event_types_.response
class type biggest_event = Event_types_.biggest_event
type 'msg result = 'msg Event_types_.result

val response : 'msg result -> response
val message : 'msg result -> 'msg option

val return : response -> 'msg result

val emit : ?response:response -> 'msg -> 'msg result

val handled : 'msg result
val unhandled : 'msg result
val stop : 'msg result

val mouse_event : biggest_event Js.t -> (Dom_html.mouseEvent Js.t -> 'msg result) -> 'msg result
val keyboard_event : biggest_event Js.t -> (Dom_html.keyboardEvent Js.t -> 'msg result) -> 'msg result
val input_contents : biggest_event Js.t -> (string -> 'msg result) -> 'msg result
