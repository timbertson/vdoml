(* Utils *)
module StringMap = Map.Make(String)
module List = struct
  include List
  let intersperse sep = function
    | [] as blank -> blank
    | _::[] as single -> single
    | first::lst ->
      lst |> List.fold_left (fun acc item ->
        item :: sep :: acc
      ) [first] |> List.rev

  let rec all fn = function
    | [] -> true
    | item::remaining ->
      if fn item then all fn remaining else false
end

(* Model *)
module Entry = struct
  type id = int
  type t = {
    name: string;
    completed: bool;
    editing: bool;
    id: int;
  }
  type msg =
    | Editing of bool
    | Rename of string
    | Toggle_check

  let create name id = {
    name; id;
    completed = false;
    editing = false;
  }

  let update = function
    | Editing editing -> fun entry -> { entry with editing }
    | Rename name -> fun entry -> { entry with name }
    | Toggle_check -> fun entry -> { entry with completed = not entry.completed }

  let is_completed e = e.completed
  let is_active e = not (is_completed e)
  let has_id id e = e.id = id
end

module Visibility = struct
  type t = [
    | `All
    | `Active
    | `Completed
  ]

  let eq : t -> t -> bool = Pervasives.(=)

  let all_visibilities : t list = [ `All; `Active; `Completed ]

  let fragment_of_visibility : t -> string = function
    | `All -> "#/"
    | `Active -> "#/active"
    | `Completed -> "#/completed"

  let visibility_of_fragment =
    let add map visibility =
      StringMap.add (fragment_of_visibility visibility) visibility map
    in
    let map = List.fold_left add StringMap.empty all_visibilities in
    fun fragment ->
      try Some (StringMap.find fragment map) with Not_found -> None

  let string_of_visibility = function
    | `All -> "All"
    | `Active -> "Active"
    | `Completed -> "Completed"
end

module Msg = struct
  type t =
    | Update_field of string
    | Modify of (Entry.id * Entry.msg)
    | Delete of Entry.id
    | Add
    | Delete_complete
    | Check_all of bool
    | Set_visibility of Visibility.t
end

open Vdoml
module Controls = struct
  open Html

  let count count =
    let suffix = if count = 1 then "item" else "items" in
    span ~a:[ a_class ["todo-count"]] [
      strong [ pcdata (string_of_int count) ];
      pcdata (" " ^ suffix ^ " left");
    ]

  let filters instance =
    let open Visibility in
    let set_visibility target =
      let onclick = Ui.Handler.emit instance (Msg.Set_visibility target) in
      let href = fragment_of_visibility target in
      let description = string_of_visibility target in
      fun current ->
        li ~a:[ a_onclick onclick ] [
          a ~a:[
            a_href href;
            a_class (if target = current then ["selected"] else []);
          ] [ pcdata description ]
        ] in
    let visibility_buttons =
      let button_fns = Visibility.all_visibilities
        |> List.map (fun v -> set_visibility v) in
      fun current ->
        button_fns
          |> List.map (fun view -> view current)
          |> List.intersperse (pcdata " ")
    in
  fun visibility ->
    ul ~a:[ a_class ["filters"] ] (visibility_buttons visibility)

  let clear_completed instance = 
    let clear = Ui.Handler.emit instance Msg.Delete_complete in
    function
      | 0 ->
        print_endline ("completed: 0");
        span []
      | num_complete ->
        print_endline ("completed: SOME");
        button ~a:[
          a_class ["clear-completed"];
          a_onclick clear;
        ] [
          pcdata ("Clear completed (" ^ (string_of_int num_complete) ^ ")")
        ]

  let view instance =
    let filters = filters instance in
    let clear_completed = clear_completed instance in
  fun visibility entries ->
    let num_entries = List.length entries in
    let num_completed = entries |>
      List.filter Entry.is_completed |>
      List.length in
    let num_remaining = num_entries - num_completed in
    if num_entries = 0
      then footer []
      else footer ~a:[a_class ["footer"]] [
        count num_remaining;
        filters visibility;
        clear_completed num_completed;
      ]
end

module App = struct
  type t = {
    entries : Entry.t list;
    new_value: string;
    uid: int;
    visibility: Visibility.t;
  }
  let initial = {
    entries = [];
    visibility = `All;
    new_value = "";
    uid = 0;
  }

  let default = function
    | Some saved -> saved
    | None -> initial

  let update model =
    let open Msg in
    let open Entry in
    let modify_entry fn id entry = if id = entry.id then fn entry else entry in
    let modify_entries fn = {
      model with entries = fn model.entries
    } in
    let map_entries fn = modify_entries (List.map fn) in
    let filter_entries fn = modify_entries (List.filter fn) in
  function
    | Add ->
        if model.new_value = "" then model else (
          let uid = model.uid in
          let text = model.new_value in
          { model with
            uid = uid + 1;
            new_value = "";
            entries = Entry.create text uid :: model.entries;
          }
        )
    | Update_field text -> { model with new_value = text }
    | Delete id -> filter_entries (fun entry -> entry.id <> id)
    | Delete_complete -> filter_entries Entry.is_active
    | Check_all completed -> map_entries (fun entry -> { entry with
      completed = completed
    })
    | Modify (id, msg) -> { model with
      entries = model.entries
        |> List.map (modify_entry (Entry.update msg) id)
    }
    | Set_visibility vis -> { model with visibility = vis }

  module View = struct
    open Html
    open Msg

    let a_onreturnkey (fn : ('b #Dom.event as 'a) Js.t -> event_response) =
      a_onkeydown (fun event ->
        if event##.keyCode == 13 then fn (event :> Dom_html.event Js.t) else `Unhandled
      )

    let view_input instance =
      let onchange = (fun evt ->
        let text = Input.contents evt in
        print_endline ("changed to: " ^ text);
        Ui.emit instance (Update_field text);
        `Unhandled
      ) in
      let submit = (fun _ ->
        Ui.emit instance Add;
        `Handled
      ) in
    fun task ->
      header ~a: [a_class ["header"]] [
        h1 [ pcdata "todos" ];
        input ~a: [
          a_class ["new-todo"];
          a_placeholder "What needs to be done?";
          a_autofocus `Autofocus;
          a_value task;
          a_name "newTodo";
          Input.a_oninput onchange;
          a_onreturnkey submit;
        ] ()
      ]

    let view_entry instance =
      let open Html in
      let open Msg in
      let open Entry in
    fun entry ->
      let emitter signal = Ui.Handler.emit instance signal in
      let toggle_class name value = if value then [name] else [] in
      let toggle attr value = if value then [attr] else [] in
      (* TODO: Curry *)
      let cancel_editing = Ui.Handler.emit instance (
        Modify (entry.id, Editing false)) in
      let rename = fun e ->
        let text = Input.contents e in
        Ui.emit instance (Modify (entry.id, Rename text));
        `Unhandled
      in

      li
        ~a:[ a_class (
          (toggle_class "completed" entry.completed) @
          (toggle_class "editing" entry.editing)
        )]
        [
          div ~a:[ a_class ["view"] ] [
            input ~a:(
              toggle (a_checked `Checked) entry.completed @
              [
                a_class ["toggle"];
                a_input_type `Checkbox;
                (* TODO is this something where lazy_n_ might help? *)
                (* a_onclick (emitter (Modify (entry.id, Toggle_check))); *)
                a_onclick (fun _evt ->
                  Ui.emit instance (Modify (entry.id, Toggle_check));
                  `Unhandled);
              ]
            ) ();
            label ~a:[
                a_on "dblclick" (emitter (Modify (entry.id, Editing true)));
                (* on_doubleclick (emitter (EditingEntry entry.id, true)) *)
              ] [ pcdata entry.name ];
            button
              ~a:[
                a_class ["destroy"];
                a_onclick (emitter (Delete entry.id))
              ] []
          ];
          input
              ~a:[
                a_class ["edit"];
                a_value entry.name;
                a_name "title";
                a_id ("todo-" ^ (string_of_int entry.id));
                Input.a_onchange rename;
                a_onblur cancel_editing;
                a_onreturnkey cancel_editing;
              ] ()
      ]


    let view_entries instance =
      let view_entries = Ui.collection ~view:view_entry
        ~id:(fun state -> `Int state.Entry.id)
        instance
      in
      let checked flag attrs =
        if flag then (a_checked `Checked) :: attrs else attrs
      in
    fun visibility ->
      let is_visible = match visibility with
        | `All -> fun _ -> true
        | `Completed -> Entry.is_completed
        | `Active -> Entry.is_active
      in
    function
      | [] -> div []
      | entries -> (
        let toggle_all = Curry.init (fun all_completed ->
          fun _ ->
            Ui.emit instance (Check_all (not all_completed));
            `Unhandled
        ) in
        let all_completed = List.all Entry.is_completed entries in
        section ~a:[
          a_class ["main"];
        ] [
          input ~a:(checked all_completed [
            a_class ["toggle-all"];
            a_input_type `Checkbox;
            a_name "toggle";
            a_onclick (Curry.apply toggle_all all_completed);
          ]) ();
          label ~a:[
            a_for "toggle-all";
          ] [ pcdata "Mark all as complete" ];
          ul ~a:[a_class ["todo-list"]]
            (entries |> List.filter is_visible |> view_entries)
        ]
      )

    let info_footer =
      footer ~a:[ a_class ["info"] ] [
        p [ pcdata "Double-click to edit a todo" ];
        p [
            pcdata "Written by ";
            a ~a:[ a_href "https://github.com/evancz" ] [ pcdata "Evan Czaplicki" ];
        ];
        p [
          pcdata "Part of ";
            a ~a:[ a_href "http://todomvc.com" ] [ pcdata "TodoMVC" ]
        ];
      ]

    let view instance =
      let view_input = view_input instance in
      let view_entries = view_entries instance in
      let view_controls = Controls.view instance in
    fun state -> (
      div ~a:[
        a_class ["todomvc-wrapper"];
      ] [
        section ~a:[ a_class ["todoapp"] ] [
          view_input state.new_value;
          view_entries state.visibility state.entries;
          view_controls state.visibility state.entries;
        ];
        info_footer
      ])
  end

  let build state = Ui.component ~update ~view:View.view (default state)
end

let () =
  let root () =
    let doc = Dom_html.document in
    Js.Opt.get (doc##getElementById (Js.string "todomvc")) (fun () -> assert false)
  in
  let ui = App.build None in
  Ui.onload (Ui.main ~log:Logs.Debug ~root ui)
