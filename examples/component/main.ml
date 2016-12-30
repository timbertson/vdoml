module Message = struct
	type t =
		| Top of Counter.message
		| Bottom of Counter.message
		| Name of Name_field.message
end

open Vdoml

module PairOfCounters = struct
	open Message
	type model = {
		top: Counter.model;
		bottom: Counter.model;
		name: Name_field.model;
	}

	let update = fun current -> function
		| Top message -> { current with top = Counter.update current.top message }
		| Bottom message -> { current with bottom = Counter.update current.bottom message }
		| Name message -> { current with name = Name_field.update current.name message }

	let view (instance: (model, Message.t) Ui.instance) =
		let top_view = Ui.child ~message:(fun msg -> Top msg) Counter.component instance in
		let bottom_view = Ui.child ~message:(fun msg -> Bottom msg) Counter.component instance in
		let name_view = Ui.child ~message:(fun msg -> Name msg) Name_field.component instance in
		fun { top; bottom; name } ->
			let open Html in
			div [ top_view top; bottom_view bottom; name_view name ]

	let build () = Ui.root_component ~update ~view {
		top = Counter.initial;
		bottom = Counter.initial;
		name = Name_field.initial;
	}
end

let () = Ui.onload (Ui.main
	~root:"main"
	(PairOfCounters.build ())
)
