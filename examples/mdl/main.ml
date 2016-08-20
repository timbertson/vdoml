(* create a Hook module which registers all vdom-created elements with mdl's componentHandler *)
module Mdl_dom : Ui_f.DOM_HOOKS = struct
	let componentHandler = lazy (Js.Unsafe.(get global "componentHandler"))
	let register_element (e: Dom_html.element Js.t) =
		Js.Unsafe.meth_call (Lazy.force componentHandler) "upgradeElement" [| Js.Unsafe.inject e |]
	let unregister_element _e = ()
end

(* Make a non-default version of Ui using our hooks *)
module Ui = Ui_f.Make(Mdl_dom)

module App = struct
	let next_id = ref 0
	type item = { id: int; name: string }
	type model = { items: item list }
	type message =
		| Add
		| Edit of int * string
	let init = { items = [] }
	let update state = function
		| Add ->
			let id = !next_id in
			next_id := id + 1;
			{ items = state.items @ [ { id; name = ""; } ] }
		| Edit (id, contents) ->
			{ items = state.items |> List.map (fun item ->
				if item.id = id
					then { item with name = contents }
					else item
			)}

	open Html

	let view_item instance =
		let update = Input.lift @@ Ui.handler instance ~response:`Unhandled @@ fun item evt ->
			Edit (item.id, Input.contents evt)
		in
		fun { id; name } ->
			let id = "item-" ^ (string_of_int id) in
			li ~a:[ s_class "mdl-list__item"; ] [
				div ~a:[
					s_class "mdl-textfield mdl-js-textfield";
				] [
					input ~a:[
						s_class "mdl-textfield__input";
						a_input_type `Text;
						a_id id;
						a_onchange update;
						a_value name;
					] ();
					label ~a:[s_class "mdl-textfield__label"; a_for id] [ text ("lbl for: " ^ name) ];
				]
			]

	let view instance =
		let add = Ui.emitter instance Add in
		let abort = (fun _ -> Ui.abort instance; `Handled) in
		let view_items = Ui.collection
			~view:(view_item)
			~id:(fun item -> `Int item.id)
			instance
		in
		fun { items } ->
			div [
				h1 [ text "Items:" ];
				ul ~a:[ s_class "mdl-list"; ] (items |> view_items);
				button ~a:[
					s_class "mdl-button mdl-js-button mdl-button--raised";
					a_onclick add
				] [ text "add" ];

				button ~a:[
					s_class "mdl-button mdl-button-primary mdl-js-button mdl-button--raised";
					a_onclick abort;
				] [ text "BOOM" ];
			]

	let build : (Html5_types.div_content_fun, model, message) Ui.component = Ui.component ~update ~view init
end

let () =
	let ui = App.build in
	Ui.onload (Ui.main ~root:"main" ui)
