open Test_util
include Init
open Vdom_
open Html
open Sexplib
open Diff
open Log_

module Html_gen = struct
	let rand = lazy (
		let seed = try
			let seed = int_of_string (Unix.getenv "RANDOM_SEED") in
			Printf.eprintf "Used existing RANDOM_SEED=%d\n" seed;
			seed
		with Not_found ->
			(* Initialize just to get a random seed :D *)
			Random.self_init ();
			let seed = Random.int (2 lsl 20) in
			Printf.eprintf "[ Generated new RANDOM_SEED=%d ]\n" seed;
			seed
		in
		Random.State.make [|seed|]
	)

	let pick lst =
		let len = List.length lst in
		List.nth lst (Random.State.int (Lazy.force rand) len)
	
	type elem = [ `div | `input | `span | `p | `input | `text ]
	let digit () : int = pick [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
	let element () =
		let attr = "contents_" ^ (string_of_int (digit ())) in
		let id = pick [ Some (`Int (digit ())) ; None ] in
		let raw = match pick [`div;`input;`span;`p;`input;`text] with
			| `div -> div ~a:[a_title attr] []
			| `input -> input ~a:[a_title attr] ()
			| `span -> span ~a:[a_title attr] []
			| `p -> p ~a:[a_title attr] []
			| `text -> text attr
		in
		Vdom.Internal.root (match id with
			| Some id -> Ui.identify id raw
			| None -> raw
		)
	
	let children () =
		let rec gen acc = function
			| 0 -> acc
			| n -> (element ()) :: (gen acc (n-1))
		in
		gen [] (pick [ 0; 2; 3; 5; 7 ])
end

module Diff_util = struct
	open Vdom
	open Sexp
	type raw_node = unit Vdom.raw_node
	type node = unit Vdom.node
	type node_mutation = unit Diff.node_mutation
	type node_mutations = node_mutation list
	let raw = raw_of_node
	let node : unit Vdom.Internal.pure_node -> unit Vdom.Internal.node = Vdom.Internal.root
	type mutation_mode = [ `id | `anon ]

	let sexp_of_raw_node node =
		Atom (string_of_raw node)
	let compare_raw_node = Pervasives.compare

	let sexp_of_identity = function
		| User_tag (`String s) -> Atom s
		| User_tag (`Int i) -> Atom (string_of_int i)
		| Internal_tag i -> Atom (string_of_int i)

	let sexp_of_node node =
		match node with
			| Anonymous node -> List [ Atom "Anonymous"; sexp_of_raw_node node ]
			| Identified (id, node) -> List [ Atom "Identified"; sexp_of_identity id; sexp_of_raw_node node ]
	let compare_node = Pervasives.compare

	let sexp_of_node_mutation =
		let s x = Atom x and l x = List x in
		function
			| Update ( a, b ) -> l [ s "Update"; s (string_of_raw a); s (string_of_raw b) ]
			| Insert a -> l [ s "Insert"; s (string_of_node a) ]
			| Append a -> l [ s "Append"; s (string_of_node a) ]
			| Remove a -> l [ s "Remove"; s (string_of_raw a) ]

	let string_of_node_list nodes = "["^( String.concat ", " (List.map string_of_node nodes) )^"]"
	let sexp_of_node_mutations m = Sexp.List (List.map sexp_of_node_mutation m)
	let compare_node_mutations = Pervasives.compare

	let assert_mutations ~existing ~replacements expected =
		let mutations = ref [] in
		let apply = (fun mutation state ->
			ignore state;
			mutations := mutation :: !mutations
		) in

		Diff.update_children_pure ~apply existing replacements;

		let mutations = List.rev !mutations in
		[%test_result:node_mutations] ~expect:expected mutations

	let virtual_apply initial =
		let nodes = ref initial in

		let split_nodes state =
			Log.debug (fun m->m "splitting nodes on idx %d: %s"
				state.dom_idx
				(string_of_node_list !nodes)
			);
			let rec loop = fun before after -> function
				| 0 ->
					Log.debug (fun m->m "split result: %s // %s"
						(string_of_node_list before)
						(string_of_node_list after)
					);
					(before, after)
				| n -> (match after with
					| current::tail -> loop (before @ [current]) tail (n-1)
					| [] -> failwith "premature end of list"
				)
			in
			loop [] !nodes state.dom_idx
		in

		let apply = fun mutation state ->
			let new_nodes = (match mutation with
				| Update (existing, replacement) -> (
					match split_nodes state with
						| pre, current::remaining -> (
							let current = match current with
							| Identified (id, _existing) ->
								[%test_result:raw_node] ~expect:_existing existing;
								Identified (id, replacement)

							| Anonymous _existing ->
								[%test_result:raw_node] ~expect:_existing existing;
								Anonymous replacement
							in
							pre @ [current] @ remaining
						)
						| _ -> assert false
				)
				| Insert addition ->
					let pre, remaining = split_nodes state in
					pre @ [addition] @ remaining
				| Append addition -> 
					let pre, remaining = split_nodes state in
					[%test_result:node list] ~expect:[] remaining;
					pre @ remaining @ [addition]
				| Remove node -> (
					match split_nodes state with
						| _, [] -> failwith "remove with no remaining nodes"
						| pre, current::remaining ->
							[%test_result:raw_node] ~expect:(raw current) node;
							pre @ remaining
				)
			) in
			Log.debug (fun m->m "after applying %s, nodes = %s"
				(string_of_node_mutation mutation)
				(string_of_node_list new_nodes)
			);
			nodes := new_nodes
		in
		let extract () = !nodes in
		(apply, extract)
end
open Diff_util

let first_div = node @@ div ~a:[a_class "first"] []
let first_span = node @@ span ~a:[a_class "first"] []
let first_p1 = node @@ p ~a:[a_class "first p1"] []
let first_p2 = node @@ p ~a:[a_class "first p2"] []
let first_p3 = node @@ p ~a:[a_class "first p3"] []

let second_div = node @@ div ~a:[a_class "second"] []
let second_span = node @@ span ~a:[a_class "second"] []
let second_p1 = node @@ p ~a:[a_class "second p1"] []
let second_p2 = node @@ p ~a:[a_class "second p2"] []
let second_p3 = node @@ p ~a:[a_class "second p3"] []

let first_id1 = node @@ Ui.identify (`Int 1) @@ div ~a:[a_class "first id1"] []
let first_id2 = node @@ Ui.identify (`Int 2) @@ div ~a:[a_class "first id2"] []
let first_id3 = node @@ Ui.identify (`Int 3) @@ div ~a:[a_class "first id3"] []

let second_id1 = node @@ Ui.identify (`Int 1) @@ div ~a:[a_class "second id1"] []
let second_id2 = node @@ Ui.identify (`Int 2) @@ div ~a:[a_class "second id2"] []
let second_id3 = node @@ Ui.identify (`Int 3) @@ div ~a:[a_class "second id3"] []

let%TEST_UNIT "removed element" =
	assert_mutations
		~existing:[ first_div; first_span ]
		~replacements: [ second_span ]
		[
			Remove (raw first_div);
			Update (raw first_span, raw second_span);
		]

let%TEST_UNIT "inserted element" =
	assert_mutations
		~existing:[ first_div ]
		~replacements:[ second_span; second_div ]
		[
			Insert (second_span);
			Update (raw first_div, raw second_div);
		]

let%TEST_UNIT "multiple inserted elements" =
	assert_mutations
		~existing:[ first_div ]
		~replacements:[ second_span; second_p1; second_p2; second_div ]
		[
			Insert (second_span);
			Insert (second_p1);
			Insert (second_p2);
			Update (raw first_div, raw second_div);
		]

let%TEST_UNIT "reordered element" =
	assert_mutations
		~existing:[ first_div; first_span ]
		~replacements:[ second_span; second_div ]
		[
			Remove (raw first_div);
			Update (raw first_span, raw second_span);
			Append (second_div);
		]

let%TEST_UNIT "identified elements reordered amongst anons" =
	assert_mutations
		~existing:[ first_div; first_id1; first_span; first_id2 ]
		~replacements:[ second_span; second_id2; second_div; second_id1 ]
		[
			Insert (second_span);
			Remove (raw first_div);
			Remove (raw first_id1);
			Remove (raw first_span);
			Update (raw first_id2, raw second_id2);
			Append (second_div);
			Append (second_id1);
		]

let%TEST_UNIT "anon elements amongst identified" =
	assert_mutations
		~existing:[ first_id1; first_id2; first_span; first_id3 ]
		~replacements:[ second_id1; second_id2; second_id3; second_div ]
		[
			Update (raw first_id1, raw second_id1);
			Update (raw first_id2, raw second_id2);
			Remove (raw first_span);
			Update (raw first_id3, raw second_id3);
			Append (second_div);
		]


let%TEST_UNIT "random update" =
	let iterations = ref 50 in
	while !iterations > 0; do
		decr iterations;
		let initial = Html_gen.children () in
		let apply, get_result = virtual_apply initial in
		let replacements = Html_gen.children () in
		Log.info (fun m->m "updating initial (%s) -> final (%s)"
			(string_of_node_list initial)
			(string_of_node_list replacements)
		);
		Diff.update_children_pure ~apply initial replacements;
		[%test_result:node list] ~expect:replacements (get_result ())
	done
