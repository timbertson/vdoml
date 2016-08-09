open Test_util
include Init
open Vdom.Html

let string_of_dom elem =
	elem##.innerHTML |> Js.to_string

let string_of_vdom vdom =
	let elem = Dom_html.document##createElement(Js.string "div") in
	Vdom.Diff.init vdom elem;
	string_of_dom elem

type runner = {
	dom_string : unit -> string;
	dom_node : unit -> Dom_html.element Js.t;
	update : Vdom.node -> unit;
}

let run_vdom initial fn =
	let elem = Dom_html.document##createElement(Js.string "div") in
	Vdom.Diff.init initial elem;
	let current = ref initial in
	let runner = {
		dom_string = (fun () -> string_of_dom elem);
		dom_node = (fun () ->
			Js.Opt.bind elem##.firstChild Dom_html.CoerceTo.element |> force_opt
		);
		update = (fun vdom ->
			Vdom.Diff.update !current vdom elem;
			current := vdom
		);
	} in
	fn runner

let simple_html =
	div ~a:[a_class ["active"]] [
		pcdata "child text"
	]

let simple_html_str = "<div class=\"active\">child text</div>"

let%TEST_UNIT "simple HTML building" =
	[%test_eq:string] (string_of_vdom simple_html) simple_html_str

let%TEST_UNIT "update identical HTML" =
	run_vdom simple_html (fun runner ->
		[%test_eq:string] (runner.dom_string ()) simple_html_str;
		runner.update simple_html;
		[%test_eq:string] (runner.dom_string ()) simple_html_str;
		()
	)

let%TEST_UNIT "node type change" =
	run_vdom (div []) (fun runner ->
		[%test_eq:string] (runner.dom_string ()) "<div></div>";
		runner.update (span []);
		[%test_eq:string] (runner.dom_string ()) "<span></span>";
		()
	)

let%TEST_UNIT "property change" =
	let checkbox checked =
		let attrs = [a_input_type `Checkbox] in
		let attrs = if checked then (a_checked `Checked)::attrs else attrs in
		input ~a:attrs ()
	in
	run_vdom (checkbox false) (fun runner ->
		let is_checked () = (runner.dom_node()
			|> Dom_html.CoerceTo.input
			|> force_opt)##.checked
			|> Js.to_bool in
		[%test_eq:bool] (is_checked ()) (false);
		runner.update (checkbox true);
		[%test_eq:bool] (is_checked ()) (true);
		()
	)

