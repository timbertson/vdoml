(* utils *)
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
let log_src = Logs.Src.create "todo"
module Log = (val Logs.src_log log_src)

(* main todo model *)
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

(* module for visibility-related functions *)
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
  (* app update type *)
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

(* module for the controls at the bottom of the UI *)
module Controls = struct
  open Html

  let count count =
    let suffix = if count = 1 then "item" else "items" in
    span ~a:[ a_class ["todo-count"]] [
      strong [ text (string_of_int count) ];
      text (" " ^ suffix ^ " left");
    ]

  let filters instance =
    let open Visibility in
    let set_visibility target =
      let onclick = Ui.handler instance (fun _ _ -> Msg.Set_visibility target) in
      let href = fragment_of_visibility target in
      let description = string_of_visibility target in
      fun current ->
        li ~a:[ a_onclick onclick ] [
          a ~a:[
            a_href href;
            a_class (if target = current then ["selected"] else []);
          ] [ text description ]
        ] in
    let visibility_buttons =
      let button_fns = Visibility.all_visibilities
        |> List.map (fun v -> set_visibility v) in
      fun current ->
        button_fns
          |> List.map (fun view -> view current)
          |> List.intersperse (text " ")
    in
  Curry.init (fun visibility ->
    ul ~a:[ a_class ["filters"] ] (visibility_buttons visibility)
  )

  let clear_completed instance = 
    let clear = Ui.handler instance (fun _ _ -> Msg.Delete_complete) in
    function
      | 0 ->
        Log.info (fun m -> m "completed: 0");
        span []
      | num_complete ->
        Log.info (fun m -> m "completed: %d" num_complete);
        button ~a:[
          a_class ["clear-completed"];
          a_onclick clear;
        ] [
          text ("Clear completed (" ^ (string_of_int num_complete) ^ ")")
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
  (* overall model type for the application *)
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

  (* Given an existing state (model) and an update message,
   * compute the next state *)
  let update model =
    let open Msg in
    let open Entry in
    let modify_entry fn id entry = if id = entry.id then fn entry else entry in
    let modify_entries fn = {
      model with entries = fn model.entries
    } in
    let map_entries fn = modify_entries (List.map fn) in
    let filter_entries fn = modify_entries (List.filter fn) in
    (function
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
      | Modify (id, msg) -> modify_entries (
        List.map (modify_entry (Entry.update msg) id)
      )
      | Set_visibility vis -> { model with visibility = vis }
    )

  module View = struct
    open Html
    open Msg

    (* view functions accept a Ui instance and the current state.
     * they return a pure (virtual) HTML structure which
     * is diffed against the previous value to udpdate the real DOM *)

    (* filter keyboard events to just return keys *)
    let ifreturnkey (fn : ('b #Dom.event as 'a) Js.t -> event_response) = (fun event ->
      if event##.keyCode == 13
        then fn (event :> Dom_html.event Js.t)
        else `Unhandled
    )

    (* view for the top input (new task entry) *)
    let view_new_task instance =
      let onchange = Input.lift (fun evt ->
        let text = Input.contents evt in
        Log.info(fun m -> m "input changed to: %s" text);
        Ui.emit instance (Update_field text);
        `Unhandled
      ) in
      let onkeydown = ifreturnkey (Ui.emitter instance Add) in
    fun task ->
      header ~a: [a_class ["header"]] [
        h1 [ text "todos" ];
        input ~a:[
          a_class ["new-todo"];
          a_placeholder "What needs to be done?";
          a_autofocus `Autofocus;
          a_value task;
          a_name "newTodo";
          a_oninput onchange;
          a_onkeydown onkeydown;
        ] ()
      ]

    (* view function for an indiidual todo entry *)
    let view_entry instance =
      let open Html in
      let open Msg in
      let open Entry in
      let open Ui in
      let toggle_class name value = if value then [name] else [] in
      let if_attr a = function true -> a | false -> None in
      let cancel_editing = handler instance (fun entry _evt ->
        Modify (entry.id, Editing false)
      ) in
      let onkeydown = ifreturnkey cancel_editing in
      let rename = Input.lift @@ handler instance ~response:`Unhandled (fun entry e ->
        let text = Input.contents e in
        Modify (entry.id, Rename text)
      ) in
      let toggle_check = handler instance ~response:`Unhandled (fun entry _evt ->
        Modify (entry.id, Toggle_check)
      ) in
      let delete = handler instance (fun entry _evt -> Delete entry.id) in
      let start_editing = handler instance (fun entry _evt -> Modify (entry.id, Editing true)) in
      (fun entry ->
        li ~a:[
          a_class (
            (toggle_class "completed" entry.completed) @
            (toggle_class "editing" entry.editing))
        ] [
          div ~a:[ a_class ["view"] ] [
            input ~a:[
              if_attr (a_checked `Checked) entry.completed;
              a_class ["toggle"];
              a_input_type `Checkbox;
              a_onclick toggle_check;
            ] ();
            label ~a:[
              a_on "dblclick" start_editing;
            ] [ text entry.name ];
            button ~a:[
              a_class ["destroy"];
              a_onclick delete;
            ] []
          ];
          input ~a:[
            a_class ["edit"];
            a_value entry.name;
            a_name "title";
            a_id ("todo-" ^ (string_of_int entry.id));
            a_onchange rename;
            a_onblur cancel_editing;
            a_onkeydown onkeydown;
          ] ()
        ]
      )


    (* view a list of entries *)
    let view_entries instance =
      (* make a collection version of the `view_entry` function,
       * which allows child views to be cached & tracked over
       * subsequent renders *)
      let view_entries = Ui.collection ~view:view_entry
        ~id:(fun state -> `Int state.Entry.id)
        instance
      in
      let checked = function true -> a_checked `Checked | false -> None in
      let all_completed entries = List.all Entry.is_completed entries in
      let toggle_all = Ui.handler instance ~response:`Unhandled (fun state _evt ->
        Check_all (not (all_completed state.entries));
      ) in
      fun visibility ->
        let is_visible = match visibility with
          | `All -> fun _ -> true
          | `Completed -> Entry.is_completed
          | `Active -> Entry.is_active
      in
      function
        | [] -> div []
        | entries -> (
          section ~a:[
            a_class ["main"];
          ] [
            input ~a:[
              checked (all_completed entries);
              a_class ["toggle-all"];
              a_input_type `Checkbox;
              a_name "toggle";
              a_onclick toggle_all;
            ] ();
            label ~a:[
              a_for "toggle-all";
            ] [ text "Mark all as complete" ];
            ul ~a:[a_class ["todo-list"]]
              (entries |> List.filter is_visible |> view_entries)
          ]
        )

    let info_footer =
      footer ~a:[ a_class ["info"] ] [
        p [ text "Double-click to edit a todo" ];
        p [
            text "Written by ";
            a ~a:[ a_href "https://github.com/timbertson" ] [ text "Tim Cuthbertson" ];
        ];
        p [
          text "Part of ";
            a ~a:[ a_href "http://todomvc.com" ] [ text "TodoMVC" ]
        ];
      ]

    (* overall app view function *)
    let view instance =
      let view_new_task = view_new_task instance in
      let view_entries = view_entries instance in
      let view_controls = Controls.view instance in
      (fun state ->
        div ~a:[
          a_class ["todomvc-wrapper"];
        ] [
          section ~a:[ a_class ["todoapp"] ] [
            view_new_task state.new_value;
            view_entries state.visibility state.entries;
            view_controls state.visibility state.entries;
          ];
          info_footer
        ]
      )
  end

  (* build a new app component using update + view functions plus initial state *)
  let build state = Ui.component ~update ~view:View.view (default state)
end

let () =
  Logs.(Src.set_level log_src (Some Warning));
  let root () = Js.Opt.get
    (Dom_html.document##getElementById (Js.string "todomvc"))
    (fun () -> assert false)
  in
  let ui = App.build None in
  Ui.onload (Ui.main ~log:Logs.Debug ~root ui)
