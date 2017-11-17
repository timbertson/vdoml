open Vdoml
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
		Html.text text
	let component text =
		Ui.root_component ~view ~update (0, text)
end

type 'evt runner = {
	emit: 'evt -> unit;
	view: unit -> 'evt Html.html;
}

let runner component fn =
	run_component component (fun instance ->
		let view_fn = update_and_view instance in
		let state = ref component.root_init in
		let emit message = state := component.root_update !state message in
		fn {
			emit; view = fun () -> view_fn !state
		}
	)


let%test_unit "update_and_view unchanged" =
	runner (Text.component "hi") (fun widget ->
		let initial = widget.view () in

		(* note: physical equality *)
		[%test_eq:bool] (initial == widget.view ()) true;

		widget.emit `Noop;
		[%test_eq:bool] (initial == widget.view ()) true
	)

let%test_unit "update_and_view changed" =
	runner (Text.component "hi") (fun widget ->
		let initial = widget.view () in

		widget.emit `Increment;
		[%test_eq:bool] (initial == widget.view ()) false;

		(* .. but they should be structurally equal! *)
		[%test_eq:bool] (initial = widget.view ()) true;

		(* Once render output changes, they should be completely different *)
		widget.emit (`Text "hello!");
		[%test_eq:bool] (initial = widget.view ()) false
	)

