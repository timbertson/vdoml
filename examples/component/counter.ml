open Vdoml

type model = int
type message = Increment | Decrement

let update = fun current -> function
	| Increment -> current + 1
	| Decrement -> current - 1

let view instance state =
	let open Html in
	div [
		button ~a:[a_onclick (emitter Decrement)] [ text "-" ];
		span [ text (string_of_int state) ];
		button ~a:[a_onclick (emitter Increment)] [ text "+" ];
	]

let initial = 0
let component = Ui.component ~view ()
