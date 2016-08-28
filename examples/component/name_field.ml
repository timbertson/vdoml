open Vdoml
type model = string
type message = string

let update _ newContent = newContent

let view (instance: (model, message) Ui.instance) : model -> message Html.html =
	let oninput event =
		let input :Dom_html.inputElement Js.t =
			Js.Opt.get
				(Js.Opt.bind event##.currentTarget Dom_html.CoerceTo.input)
				(fun () -> assert false) in
		
		Some (Js.to_string input##.value)
	in
	fun state ->
		let open Html in
		div [
			input ~a:[a_oninput (emitter ~response:`Unhandled oninput); a_value state] ();
			div [ text "Hello, "; text state ];
		]

let build init = Ui.component ~update ~view init
