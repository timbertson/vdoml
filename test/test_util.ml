include Sexplib.Conv
module PTest = Ppx_test.Test
module Init = struct
	let init () = ()
end
open Vdoml
module Diff = Vdoml.Diff_.Make(Vdoml.Diff_.No_hooks)
module Ui = Ui_.Make(Vdoml.Diff_.No_hooks)
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
