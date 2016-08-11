open Vdom.Internal

module Component = struct
  type ('elt, 'a) instance_state = {
    state_val: 'a;
    state_counter: int;
    state_view: Vdom.node;
  }

  type 'message emit_fn = 'message -> unit

  and ('elt, 'model, 'message) view_fn = ('elt, 'model, 'message) instance -> 'model -> 'elt Vdom.Html.elt

  and ('elt, 'model, 'message) component = {
    init: 'model;
    update: 'model -> 'message -> 'model;
    component_view: ('elt, 'model, 'message) view_fn;
  }

  and ('elt, 'model, 'message) instance = {
    emit: 'message emit_fn;
    identity: Vdom.Pure.identity;
    instance_view: ('elt, 'model, 'message) view_fn;
    (* This is a bit lame - because we can't encode state in the vdom,
     * we store it (along with the vdom) here. Re-rendering on unchanged
     * state is only skipped if each instance is used only once in the DOM.
     * This is recommended, but can't be enforced. *)
    state: ('elt, 'model) instance_state option ref;
  }

  module State = struct
    let update view_fn current new_state =
      if current.state_val = new_state
        then current
        else {
          state_val = new_state;
          state_counter = current.state_counter + 1;
          state_view = view_fn new_state;
        }

    let init dom state = { state_val = state; state_counter = 0; state_view = dom }
  end

  let gen_identity = let tag_counter = ref 0 in
    let open Vdom.Pure in
    function
      | Some id -> User_tag id
      | None ->
        let tag = !tag_counter in
        tag_counter := tag + 1;
        Internal_tag tag

  let wrap_message instance = function
    | None -> instance.emit
    | Some message -> (fun msg -> instance.emit (message msg))
end

let identify id = let open Vdom.Pure in identify (User_tag id)

(* reexpose at toplevel *)
type 'message emit_fn = 'message -> unit
type ('elt, 'model, 'message) view_fn = ('elt, 'model, 'message) Component.instance -> 'model -> 'elt Vdom.Html.elt

let component
    ~(update:'model -> 'message -> 'model)
    ~(view: ('elt, 'model, 'message) Component.view_fn)
    (init:'model) = 
  let open Component in
  { init; update; component_view = view }

let update_and_view instance =
    let open Component in
    let id = instance.identity in
    let render = let view = instance.instance_view instance in
      fun state -> Vdom.Pure.identify_anonymous id (view state) in
  fun state ->
    (* Note: this is effectful because we need to store `state` separate from VDOM :( *)
    let state = match !(instance.state) with
      | None -> State.init (render state) state
      | Some existing -> State.update render existing state
    in
    instance.state := Some (state);
    state.state_view

let instantiate component =
  let open Component in
  let events, emit = Lwt_stream.create () in
  let instance = {
    emit = (fun msg -> emit (Some msg));
    identity = gen_identity None;
    state = ref None;
    instance_view = component.component_view;
  } in
  (instance, events)

let render
    (component: ('elt, 'model, 'message) Component.component)
    (root:Dom_html.element Js.t)
    : ('elt, 'model, 'message) Component.instance * unit Lwt.t =
  let instance, events = instantiate component in

  let open Component in
  let run_thread = (
    let open Vdom in
    let view_fn = update_and_view instance in
    (* NOTE: we're duplicating state and view here
     * because we can't trust the user not to mess
     * with it by rendering an instance twice *)
    let state = ref component.init in
    let view = ref (view_fn !state) in
    try%lwt
      Diff.init !view root;
      Lwt_stream.iter (fun message ->
        let new_state = component.update !state message in
        let new_view = view_fn new_state in
        Log.info (fun m -> m "Updating view");
        Diff.update !view new_view root;
        state := new_state;
        view := new_view;
      ) events
    with e -> (
      let backtrace = Printexc.get_backtrace () in
      let err = Printexc.to_string e in
      Log.err (fun m -> m "%s\n%s" err backtrace);
      (try
        (* Diff.remove_all root; *)
        Diff.prepend (
          let open Vdom.Html in
          div [
            h1 [ pcdata "Uncaught error:" ];
            h2 [ pcdata err ];
            pre [ pcdata backtrace ];
            hr ();
          ]
        ) root;
        with _ -> ()
      );
      raise e
    )
  ) in
  (instance, run_thread)

let emit instance = instance.Component.emit

(* default implementation: just use a list *)
module ChildCache = Collection_cache.Make(Collection_cache.Child_list)

let children ~view ?message ~id instance =
  let open Vdom.Pure in
  let open Component in
  let emit = wrap_message instance message in
  let cache = ChildCache.init () in
  fun state ->
    ChildCache.use cache (fun get ->
      state |> List.map (fun state ->
        let id = User_tag (id state) in
        let child = get id (fun _ -> update_and_view {
          emit = emit;
          identity = id;
          instance_view = view;
          state = ref None;
        }) in
        child state
      )
    )

let child ~view ?message ?id instance =
  let open Component in
  let child : ('elt, 'model, 'message) instance = {
    emit = wrap_message instance message;
    identity = gen_identity id;
    instance_view = view;
    state = ref None;
  } in
  update_and_view child

module Handler = struct
  let wrap fn : 'a -> Vdom.handler_response = fun _ -> fn (); `Handled
  let emit instance message : 'a -> Vdom.handler_response = (fun _ -> emit instance message; `Handled)
end

let onload fn =
  (* Note: this should be Lwt_main.run, but no such thing in JS *)
  let (_:unit Lwt.t) = (Lwt.(Lwt_js_events.onload () >>= (fun _evt -> fn ()))) in
  ()

let main ?log ?root ?background ui () =
  let () = match log with
    | Some lvl -> Vdom.set_log_level lvl
    | None -> Vdom.init_logging ()
  in
  let root = match root with
    | Some root -> root
    | None -> (fun () -> (Dom_html.document##.body))
  in
  let background = match background with
    | Some bg -> bg
    | None -> (fun () -> Lwt.return_unit)
  in
  let instance, run_thread = render ui (root ()) in
  Lwt.join [ background (); run_thread ]

