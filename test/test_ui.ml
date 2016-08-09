open Test_util
include Init
open Ui

module Text = struct
	type msg = [ `Text of string | `Noop | `Increment ]
	type state = int * string
	let update (count, text) = function
		| `Text msg -> (count, msg)
		| `Noop -> (count, text)
		| `Increment -> (count+1, text)
	let view instance (_, text) =
		let open Vdom.Html in
		pcdata text
	let component text =
		Ui.component ~update ~view (0, text)
end

type 'evt runner = {
	emit: 'evt -> unit;
	view: unit -> Vdom.node;
}

let runner component =
	let (instance, _events) = instantiate component in
	let view_fn = update_and_view instance in
	let state = ref component.init in
	let view = ref (view_fn !state) in
	let emit message = state := component.update !state message in
	{
		emit; view = fun () -> view_fn !state
	}


let%TEST_UNIT "update_and_view unchanged" =
	let widget = runner (Text.component "hi") in
	let initial = widget.view () in

	(* note: physical equality *)
	[%test_eq:bool] (initial == widget.view ()) true;

	widget.emit `Noop;
	[%test_eq:bool] (initial == widget.view ()) true

let%TEST_UNIT "update_and_view changed" =
	let widget = runner (Text.component "hi") in
	let initial = widget.view () in

	widget.emit `Increment;
	[%test_eq:bool] (initial == widget.view ()) false;

	(* .. but they should be structurally equal! *)
	[%test_eq:bool] (initial = widget.view ()) true;

	(* Once render output changes, they should be completely different *)
	widget.emit (`Text "hello!");
	[%test_eq:bool] (initial = widget.view ()) false

