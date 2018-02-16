include Sexplib.Conv
module Init = struct
	let init () = ()
end
open Vdoml

(* Ui_.make provides access to internals compared to Vdoml.Ui *)
module Ui = Ui_.Make(Diff_.No_hooks)
module Diff = Diff_.Make(Diff_.No_hooks)
let force_opt = Diff.force_opt
let run_component component fn =
	let container = Dom_html.createDiv (Dom_html.document) in
	let instance, context = Ui.render component container in

	let cleanup () = Ui.abort instance in
	let () =
		try fn instance; cleanup ()
		with e -> (cleanup (); raise e)
	in
	()

(* XXX sholdn't this be provided by ppx_compare??? *)
let rec compare_list : 'a. ('a -> 'a -> int) -> 'a list -> 'a list -> int = (
	fun compare_val a b ->
	match (a, b) with
		| (a::atail, b::btail) ->
			let elem_diff = compare_val a b in
			if elem_diff <> 0 then elem_diff else (
				compare_list compare_val atail btail
			)
		| (a::atail, []) -> 1
		| ([], b::btail) -> -1
		| ([], []) -> 0
)

type 'a compare_fn = 'a -> 'a -> int

let compare_bool : bool compare_fn = Pervasives.compare
let compare_string : string compare_fn = Pervasives.compare
