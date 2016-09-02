open Vdoml
open Util_
type model = string
type message = string

let update _ newContent = newContent

let view (instance: (model, message) Ui.instance) : model -> message Html.html =
	let oninput event =
		let open Option in
		Event.input_contents event
			|> map (Event.emit ~response:`Unhandled)
			|> Event.optional
	in

	fun state ->
		let open Html in
		div [
			input ~a:[a_oninput (handler oninput); a_value state] ();
			div [ text "Hello, "; text state ];
		]

let build init = Ui.component ~update ~view init
