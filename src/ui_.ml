open Log_
open Vdom_
open Attr_
open Ui_main_
open Util_
open Event_chain_

module Make(Hooks:Diff_.DOM_HOOKS) = struct
  module Diff = Diff_.Make(Hooks)
  type identity = Vdom.user_tag
  type context = Ui_main.context

  let init_logging () =
    (* setup console *)
    if Logs.reporter () == Logs.nop_reporter then (
      let console = Logs_browser.console_reporter () in
      let reporter = { Logs.report = (fun src level ~over k user_msgf ->
        console.Logs.report src level ~over k (fun console_msgf ->
          user_msgf (fun ?header ?tags fmt ->
            console_msgf ?header ?tags ("%s: @[" ^^ fmt ^^ "@]")
              (Logs.Src.name src)
          )
        )
      )} in
      Logs.set_reporter reporter
    )

  let set_log_level lvl =
    init_logging ();
    Logs.Src.set_level log_src (Some lvl);
    Log.info (fun m -> m "Set log level to %s" (Logs.level_to_string (Some lvl)))

  let identify id = let open Vdom in identify (User_tag id)

  let hook ?create ?destroy node =
    Vdom.hook (Vdom.hooks ?create ?destroy ()) node

  type 'msg node = 'msg Html.html

  type ('state, 'msg) instance_state = {
    state_val: 'state;
    state_counter: int;
    state_view: 'msg Html.html;
  }

  type 'message emit_fn = 'message -> unit

  and ('state, 'message) view_fn = 'state -> 'message Html.html
  and ('state, 'message) update_fn = 'state -> 'message -> 'state
  and ('state, 'message) command_fn = 'state -> 'message -> unit Lwt.t option

  and ('state, 'message) component = {
    component_view: ('state, 'message) instance -> ('state, 'message) view_fn;
    component_command: (('state, 'message) instance -> ('state, 'message) command_fn) option;
    component_state_eq: 'state -> 'state -> bool;
  }

  and ('state, 'message) root_component = {
    component: ('state, 'message) component;
    root_update: ('state, 'message) update_fn;
    root_init: 'state;
  }

  and ('state, 'message) instance = {
    context: context;
    emit: 'message emit_fn ref;
    identity: Vdom.identity;
    view: ('state, 'message) view_fn Lazy.t;
    state_eq: 'state -> 'state -> bool;
    (* This is a bit lame - because we can't encode state in the vdom,
     * we store it (along with the vdom) here. Re-rendering on unchanged
     * state is only skipped if each instance is used only once in the DOM.
     * This is recommended, but can't be enforced. *)
    state: ('state, 'message) instance_state option ref;
  }

  module State = struct
    let update ~eq view_fn current new_state =
      if eq current.state_val new_state
        then current
        else {
          state_val = new_state;
          state_counter = current.state_counter + 1;
          state_view = view_fn new_state;
        }

    let init dom state = { state_val = state; state_counter = 0; state_view = dom }
  end

  let gen_identity = let tag_counter = ref 0 in
    let open Vdom in
    function
      | Some id -> User_tag id
      | None ->
        let tag = !tag_counter in
        tag_counter := tag + 1;
        Internal_tag tag

  let component
    ~(view: ('state, 'message) instance -> ('state, 'message) view_fn)
    ?(eq:('state -> 'state -> bool) option)
    ?(command:(('state, 'message) instance -> ('state, 'message) command_fn) option)
    ()
    = {
      component_view = view;
      component_command = command;
      component_state_eq = eq |> Option.default (=);
    }

  let root component ~update init = {
    component;
    root_init = init;
    root_update = update;
  }

  let root_component
    ~(update:('state, 'message) update_fn)
    ~(view: ('state, 'message) instance -> ('state, 'message) view_fn)
    ?(eq:('state -> 'state -> bool) option)
    ?(command:(('state, 'message) instance -> ('state, 'message) command_fn) option)
    init
    = {
      component = {
        component_view = view;
        component_command = command;
        component_state_eq = eq |> Option.default (=);
      };
      root_init = init;
      root_update = update;
    }

  let async instance th = Ui_main.async (instance.context) th

  let identity_fn x = x

  let make_instance (type message) (type state)
    ~context
    ~(emit:message emit_fn)
    ~identity
    (component: (state, message) component) =
  (
    let rec instance = lazy (
      let emit, add_observer = match component.component_command with
        | None -> (emit, lazy identity_fn)
        | Some command ->
          let observe = lazy (
            let instance = Lazy.force instance in
            let command = command instance in
            fun msg ->
              let state = match !(instance.state) with
                | Some state -> state.state_val
                | None -> raise (Assertion_error "command invoked on an instance with no state")
              in
              command state msg |> Option.may (async instance)
          ) in
          let emit msg =
            let actually_emit msg =
              (* emit and overwrite instance.emit so that
               * further calls go straight through *)
              let instance = Lazy.force instance in
              let observe = Lazy.force observe in
              (* Note: must emit before observe, otherwise state may be None *)
              let emit = fun msg -> emit msg; observe msg in
              instance.emit := emit;
              emit msg
            in
            if Lazy.is_val observe then (
              (* already forced; we can go right ahead *)
              actually_emit msg
            ) else (
              (* emit is being called during definition
               * of instance (most likely `command instance` is calling
               * `emit`). That's OK, but queue this message so we can
               * dispatch it once `observe` is actually defined
               *)
              Log.debug (fun m -> m "observe not yet initialized; enqueueing message");
              let open Lwt in
              Lwt_js.yield ()
                >>= (fun () -> return (actually_emit msg))
                |> Ui_main.async context
          ) in
          let add_observer = lazy (
            Vdom.event_node (Event_chain.observer (Lazy.force observe))
          ) in
          (emit, add_observer)
      in

      let view = lazy (
        let instance = Lazy.force instance in
        let view = component.component_view instance in
        let add_observer = Lazy.force add_observer in
        add_observer % view
      ) in

      {
        context;
        emit = ref emit;
        identity;
        state = ref None;
        state_eq = component.component_state_eq;
        view;
      }
    ) in
    Lazy.force instance
  )

  let update_and_view instance =
      let id = instance.identity in
      let view = Lazy.force instance.view in
      let render state = Vdom.identify_anonymous id (view state) in
    fun state ->
      (* Note: this is effectful because we need to store `state` separate from VDOM :( *)
      let state = match !(instance.state) with
        | None -> State.init (render state) state
        | Some existing -> State.update ~eq:instance.state_eq render existing state
      in
      instance.state := Some (state);
      state.state_view

  let emit instance = !(instance.emit)

  let supplantable fn =
    let ref = ref None in
    fun instance arg -> (
      !ref |> Option.may Lwt.cancel;
      let th =
        (* Can't have a self-recursive value, so use Lazy for indirection *)
        let rec th = lazy (fn arg |> Lwt.map (Option.may (fun msg ->
          let current = !ref in
          let th = Lazy.force th in
          if current |> Option.map ((==) th) |> Option.default false
            then emit instance msg
            else Log.info (fun m->m"Discarding message from cancelled thread")
        ))) in
        Lazy.force th in
      ref := Some (th);
      th
    )

  let supplantable_some fn = supplantable (Lwt.map (fun x -> Some x) % fn)

  let bind instance handler = (fun evt ->
    match !(instance.state) with
    | Some state -> handler state.state_val evt
    | None -> Event.unhandled
  )

  (* default implementation: just use a list *)
  module ChildCache = Collection_cache.Make(Collection_cache.Child_list)

  let children (type child_message) (type message) (type child_state) (type state)
    ~(message:child_message -> message)
    ~(id:child_state -> identity)
    (component:(child_state, child_message) component)
    (instance:(state, message) instance)
  =
    let open Vdom in
    let cache = ChildCache.init () in
    let emit, event_node = Event_chain.child message (emit instance) in
    fun state ->
      ChildCache.use cache (fun get_or_create ->
        state |> List.map (fun state ->
          let id = User_tag (id state) in
          let child = get_or_create id (fun _ -> update_and_view (make_instance
            ~context:instance.context ~emit ~identity:id component
          )) in
          child state
        )
      ) |> List.map (Vdom.transform event_node)

  let collection ~id component instance = children ~id ~message:identity component instance

  let child (type child_message) (type message) (type child_state) (type state)
    ~(message:child_message -> message)
    ?(id:identity option)
    (component:(child_state, child_message) component)
    (instance:(state, message) instance)
    : child_state -> message Html.html
  =
    let emit, event_node = Event_chain.child message (emit instance) in
    let child : (child_state, child_message) instance = (make_instance
      ~context:(instance.context) ~emit ~identity:(gen_identity id) component
    ) in
    fun state -> update_and_view child state |> Vdom.transform event_node

  module Tasks = struct
    type ('state, 'message) t = {
      sync_tasks : (('state, 'message) instance -> unit) list ref;
      async_tasks : (('state, 'message) instance -> unit Lwt.t) list ref;
    }

    (* NOTE: must be defined before `async` shadows Ui.async *)
    let attach instance t =
      let invoke t = t instance in
      !(t.sync_tasks) |> List.iter invoke;
      !(t.async_tasks) |> List.iter ((async instance) % invoke)

    let init () = {
      sync_tasks = ref [];
      async_tasks = ref [];
    }

    (* builders for the common case *)
    let of_sync t = {
      sync_tasks = ref [t];
      async_tasks = ref [];
    }

    let of_async t = {
      sync_tasks = ref [];
      async_tasks = ref [t];
    }

    let async t task = t.async_tasks := task :: !(t.async_tasks)

    let sync t task = t.sync_tasks := task :: !(t.sync_tasks)
  end

  let render
      ?(tasks: ('state, 'message) Tasks.t option)
      ({component; root_init; root_update}: ('state, 'message) root_component)
      (root:Dom_html.element Js.t)
      : ('state, 'message) instance * context =

    let context = Ui_main.init root in
    let events, emit = Lwt_stream.create () in
    let emit = fun msg -> emit (Some msg) in
    let instance = (make_instance
      ~context ~emit ~identity:(gen_identity None) component
    ) in

    tasks |> Option.may (Tasks.attach instance);

    async instance (
      let view_fn = update_and_view instance in
      let open Lwt in

      let rec apply_updates stream state dom_state =
        Lwt_stream.peek stream >>= fun _event -> (
          (* once we know there are updates, grab all available
           * and process them at once before updating the view *)
          let updates = Lwt_stream.get_available stream in
          let state = List.fold_left root_update state updates in
          let new_view = view_fn state in
          let dom_state = Diff.update dom_state new_view root in
          apply_updates stream state dom_state
        )
      in

      let dom_state = (Diff.init ~emit (view_fn root_init) root) in
      apply_updates events root_init dom_state
    );
    (instance, context)

  let wait = Ui_main.wait

  let onload fn =
    let open Lwt in
    ignore_result (Lwt_js_events.onload () >>= (fun _evt -> fn ()))

  let main ?log ?root ?get_root ?tasks component () =
    let () = match log with
      | Some lvl -> set_log_level lvl
      | None -> init_logging ()
    in
    let get_root = match get_root with
      | Some get_root -> get_root
      | None -> (
        match root with
          | None -> fun () -> (Dom_html.document##.body)
          | Some id -> fun () -> Js.Opt.get
            (Dom_html.document##getElementById (Js.string id))
            (fun () -> raise (Assertion_error ("Element with id " ^ id ^ " not found")))
      )
    in
    let instance, main = render ?tasks component (get_root ()) in
    Ui_main.wait main

  let abort instance = Ui_main.cancel instance.context
end
