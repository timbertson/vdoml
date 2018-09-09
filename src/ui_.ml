open Log_
open Vdom_
open Attr_
open Ui_main_
open Util_
open Event_chain_

(* View module hides internals of `view` function, ensuring
 * all view invocations are cached *)
module View : sig
  type ('state, 'message) t
  val init :
    skip: ('state -> 'state -> bool) ->
    ('state -> 'message Html.html) ->
    ('state, 'message) t

  val never : 'state -> 'state -> bool
  val mutate_view : ('state, 'message) t -> ('state -> 'message Html.html) -> unit
  val update : ('state, 'message) t -> 'state -> 'message Html.html
  val last_state : ('state, 'message) t -> 'state option
end = struct
  (* This is a bit lame - because we can't encode state in the vdom,
   * we store it (along with the vdom) here. Re-rendering on unchanged
   * state is only skipped if each instance is used only once in the DOM.
   * This is recommended, but can't be enforced. *)
  type ('state, 'message) state = {
    (* counter: int; *)
    mutable input: 'state;
    mutable output: 'message Html.html;
  }

  type ('state, 'message) t = {
    mutable view : 'state -> 'message Html.html;
    mutable current: ('state, 'message) state option;
    skip: 'state -> 'state -> bool;
  }

  let never a b = false

  let init ~skip view = { view; skip; current = None; }

  let last_state s = s.current |> Option.map (fun s -> s.input)

  let mutate_view s view = s.view <- view

  let update t state =
    (* Note: this is effectful because we need to store `state` separate from VDOM. *)
    let result = match t.current with
      | None ->
        let current = {
          (* counter = 0; *)
          input = state;
          output = Html.empty;
        } in

        (* Note: we guarantee that `t.current.state` is not None before first call to view *)
        t.current <- Some current;
        current.output <- t.view state;
        current

      | Some current -> (
        if t.skip current.input state
          then current
          else (
            current.input <- state;
            current.output <- t.view state;
            current
          )
      )
    in
    result.output
end


module type UI = sig
  (** Top-level user-interface functionality *)

  (** {2 Note: currying and lazy evaluation}

  Vdoml UI instances are impure, stateful objects. They store the most recent state in order to
  skip rendering unnecessarily, as well as to facilitate useful features (like `bind`
  and `command`).

  For this reason, you should generally construct instances when _defining_ your view
  function, rather than on each render. You can do this by taking advantage of Ocaml's
  explicit currying:

    Instead of defining a component's view function as:

    let view instance state =
      let button = Ui.child instance ( ... ) in
      ( ... )


    .. you should use:

    let view instance =
      let button = Ui.child instance ( ... ) in
      fun state ->
        ( ... )

    The first `view` function will construct a new `button` each time a new state is passed in.
    The second will construct a single `button` instance which will be re-rendered each time
    a new state is passed in.
  *)

  (** {2 UI Components}

    UI components consist of three main parts:
     - state: the current
     - view function: takes state and generates the current (virtual) HTML to represent this component
     - update function: takes the current state, an update message, and returns the new state. Also known as a step function,
       as it effectively implements a single "step" of the component's state machine.
     *)

  type ('state, 'message) component
  type ('state, 'message) root_component
  type ('state, 'message) instance
  type ('state, 'message) view_fn = 'state -> 'message Html.html
  type ('state, 'message) command_fn = 'state -> 'message -> unit Lwt.t option
  type 'message node = 'message Html.html

  type identity = [ `String of string | `Int of int ]
  val identify : identity -> 'msg node -> 'msg node
  (** Assign an identity to a node. This will be used by the diffing algorithm to
      track potentially-reordered nodes across susequent renders. *)

  val hook :
    ?create:(Dom_html.element Js.t -> unit) ->
    ?destroy:(Dom_html.element Js.t -> unit) ->
    'msg node -> 'msg node
  (** Register low-level creation and removal hooks on a node.

      `node` must be an element (not text),

      This should be used as a last resort for DOM interop or for
      functionality which cannot be achieved via regular means.

      `node` ought to be identified, since anonymous elements may
      be re-used for unrelated content by the VDom diff algorithm,
      which doesn't know about hooks.
    *)

  val component :
    view:(('state, 'message) instance -> ('state, 'message) view_fn) ->
    ?eq:('state -> 'state -> bool) ->
    ?command:(('state, 'message) instance -> ('state, 'message) command_fn) ->
    unit -> ('state, 'message) component

  val root_component :
    update:('state -> 'message -> 'state) ->
    view:(('state, 'message) instance -> ('state, 'message) view_fn) ->
    ?eq:('state -> 'state -> bool) ->
    ?command:(('state, 'message) instance -> ('state, 'message) command_fn) ->
    'state -> ('state, 'message) root_component

  val root :
    ('state, 'message) component ->
    update:('state -> 'message -> 'state) ->
    'state -> ('state, 'message) root_component

  val emit : ('state, 'message) instance -> 'message -> unit
  (** Emit an update message to the given instance *)

  (* like `children`, but with type 'message = `child_message` - i.e. no message conversion *)
  val collection :
    id:('child_state -> identity)
    -> ('child_state, 'message) component
    -> ('state, 'message) instance
    -> 'child_state list
    -> 'message node list

  val children :
    message:('child_message -> 'message)
    -> id:('child_state -> identity)
    -> ('child_state, 'child_message) component
    -> ('state, 'message) instance
    -> 'child_state list
    -> 'message node list

  val child :
    message:('child_message -> 'message)
    -> ?id:identity
    -> ('child_state, 'child_message) component
    -> ('state, 'message) instance
    -> ('child_state -> 'message Html.html)

  val bind : ('state, 'message) instance
    -> ('state -> 'arg -> 'msg Event.result)
    -> ('arg -> 'msg Event.result)

  module Tasks : sig
    type ('state, 'message) t
    val init : unit -> ('state, 'message) t
    val of_sync : (('state, 'message) instance -> unit) -> ('state, 'message) t
    val of_async : (('state, 'message) instance -> unit Lwt.t) -> ('state, 'message) t
    val sync : ('state, 'message) t -> (('state, 'message) instance -> unit) -> unit
    val async : ('state, 'message) t -> (('state, 'message) instance -> unit Lwt.t) -> unit
  end

  type context
  val async : ('state, 'message) instance -> unit Lwt.t -> unit
  val abort : ('state, 'message) instance -> unit
  val wait : context -> unit Lwt.t

  val supplantable : ('a -> unit Lwt.t) -> ('a -> unit Lwt.t)

  val onload : (unit -> unit Lwt.t) -> unit

  val main :
    ?log:Logs.level
    -> ?root:string
    -> ?get_root:(unit -> Dom_html.element Js.t)
    -> ?tasks:('state, 'message) Tasks.t
    -> ('state, 'message) root_component
    -> unit -> unit Lwt.t

  (** {2 Advanced API} *)

  val render :
    ?tasks:('state, 'message) Tasks.t
    -> ('state, 'message) root_component
    -> Dom_html.element Js.t
    -> ('state, 'message) instance * context
    (** Begin a render lifecycle for a toplevel component.
        Returns the Ui instance corresponding to the component,
        as well as a {!context} representing the lifecycle
        of this component tree. *)

  val init_logging: unit -> unit
  (** ensure logging is initialized. Ui.main will call this for you *)

  val set_log_level: Logs.level -> unit

end

module Make(Hooks:Diff_.DOM_HOOKS) = struct
  module Diff = Diff_.Make(Hooks)
  type identity = Vdom.user_tag
  type context = Ui_main.context

  let _set_log_level lvl =
    Logs.Src.set_level log_src (Some lvl);
    Log.app (fun m -> m "Set log level to %s" (Logs.level_to_string (Some lvl)))

  let user_override_level = (
    (* setup default log level *)
    let url = Js.to_string Dom_html.window##.location##.href in
    (* Log.app (fun m -> m"Checking URL: %s" url); *)
    let open Logs in
    Regexp.search (Regexp.regexp "vdomlLogLevel=(debug|info|warn|error)") url 0
      |> Option.bind (function (_idx, result) -> (match Regexp.matched_group result 1 with
        | Some "debug" -> Some Debug
        | Some "info" -> Some Info
        | Some "warn" -> Some Warning
        | Some "error" -> Some Error
        | other -> None
      )
    )
  )

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
    );
    (* setup default log level *)
    user_override_level |> Option.may (_set_log_level)

  let set_log_level lvl =
    init_logging ();
    _set_log_level lvl

  let identify id = let open Vdom in identify (User_tag id)

  let hook ?create ?destroy node =
    Vdom.hook (Vdom.hooks ?create ?destroy ()) node

  type 'msg node = 'msg Html.html

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
    view: ('state, 'message) View.t;
  }

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

  let emit instance msg = !(instance.emit) msg

  let view instance = View.update (instance.view)

  let identity_fn x = x

  type ('state, 'msg) command_notification =
    | Notify_fn of ('msg -> unit)
    | Queued_messages of 'msg list

  let make_instance (type message) (type state)
    ~context
    ~(emit:message emit_fn)
    ~identity
    (component: (state, message) component) =
  (
    (* Instance initialization is complex, and self-recursive. Issues include:
     *
     *  - Defining the `emit` function relies on bnding the component's `command` function
     *    to `instance`, which is self-recursive and may have side effects like emitting
     *    initial messages. The command function should be done exactly once for each instance.
     *
     *  - A component's command function cannot be notified of an emitted event
     *    until after the first `view` has completed, otherwise the instance's
     *    last known state is undefined.
     *
     *  - The view function _also_ needs to know the component's command function,
     *    in order to wrap the rendered view in a Vdom observer.
     *
     * To deal with this, instance is a `lazy` value, accessible from within
     * its own definition. Functions which rely on knowing the value of `instance`
     * are set up to be manually lazy - the initial implementation depends on nothing.
     * On first invocation, it evaluates all dependencies, constructs a new function
     * closing over dependent values directly, and then overwrites instance.field
     * with this new, faster function so that initialization is only performed once.
     *)

    let rec instance = lazy (
      Log.warn (fun m -> m "make_instance(%s)" (Vdom.Identity.to_string identity));

      let render view_fn = Vdom.identify_anonymous identity % view_fn in

      let skip = component.component_state_eq in

      let (emit, view) = match component.component_command with
        | None -> (
          let view = View.init ~skip (fun state ->
            let instance = Lazy.force instance in
            let view_fn = render (component.component_view instance) in
            (* self-modify and re-call *)
            View.mutate_view instance.view view_fn;
            view_fn state
          ) in
          (emit, view)
        )
        | Some command -> (
          (* instance.emit notifies `command directly, `Vdom.observe` is used to wrap the
           * vdom view in an observer virtual node. This ensures that both code paths
           * (instance.emit and emitting via a DOM property) will notify `command` *)

          let initialized_command = ref (Queued_messages []) in

          let emit = (fun msg ->
            (* Once `initialized_command` is a real command, we
             * can close over its contents in a new `emit` function
             * because we know it'll never change. While we're queueing
             * messages, we'll execute this check each time *)
            match !initialized_command with
              | Notify_fn call_command -> (
                let emit: message -> unit = (fun msg ->
                  emit msg;
                  call_command msg
                ) in
                let instance = (Lazy.force instance) in
                (* self-modify and call *)
                instance.emit := emit;
                emit msg
              )
              | Queued_messages msgs -> (
                Log.debug (fun m -> m "instance not yet rendered; enqueueing command notification");
                initialized_command := Queued_messages (msg :: msgs);
                emit msg
              )
          ) in

          let view = View.init ~skip (fun state ->
            let instance = Lazy.force instance in
            let component_view = component.component_view instance in

            (* during rendering, we know instance.view.last_state
             * must be defined (it's set pre-render),
             * so we can safely initialize command notification *)
            let call_command = (match !initialized_command with
              | Notify_fn _ -> raise (Assertion_error "command already initialized at first view")
              | Queued_messages msgs -> (
                let bound_command = command instance in
                let call_command = (fun msg ->
                  let state = View.last_state instance.view |> Option.default_fn
                    (fun () -> raise (Assertion_error "view.last_state not set at render time")) in
                  bound_command state msg |> Option.may (async instance)
                ) in
                msgs |> List.rev |> List.iter call_command;
                call_command
              )
            ) in
            initialized_command := Notify_fn call_command;

            let view_fn = render (fun state ->
              Vdom.observe call_command (component_view state)
            ) in

            (* self-modify and call *)
            View.mutate_view instance.view view_fn;
            view_fn state
          ) in
          (emit, view)
        )
      in

      { context; identity; view = view; emit = ref emit; }
    ) in
    Lazy.force instance
  )

  let supplantable fn =
    let ref = ref None in
    fun arg -> (
      !ref |> Option.may Lwt.cancel;
      let th = fn arg in
      ref := Some th;
      th
    )

  let bind instance handler = (fun evt ->
    match View.last_state instance.view with
    | Some state -> handler state evt
    | None -> raise (Assertion_error
      ("bound event handler called on instance (id=" ^ (Vdom.Identity.to_string instance.identity) ^ ") with no state")
    )
  )

  let embed_child_view (type child_message) (type message) (type state)
    (map: child_message -> message)
    (view: (state, child_message) view_fn)
    : (state, message) view_fn
  = (
    fun state -> Vdom.Conversion (map, view state)
  )

  let child_instance (type child_message) (type message) (type child_state) (type state)
    ~(message:child_message -> message)
    ?(id:identity option)
    (component:(child_state, child_message) component)
    (instance:(state, message) instance)
    : (child_state, message) instance
  =
    let child : (child_state, child_message) instance = (make_instance
      ~context:(instance.context)
      ~emit:((emit instance) % message)
      ~identity:(gen_identity id)
      component
    ) in

    (* no point skipping render, child instance will do that already *)
    let view = View.init ~skip:View.never (
      Vdom.identify_anonymous child.identity % (embed_child_view message (view child))
    ) in
    {
      context = child.context;
      emit = ref (emit instance);
      identity = child.identity;
      view = view;
    }

  let child (type child_message) (type message) (type child_state) (type state)
    ~(message:child_message -> message)
    ?(id:identity option)
    (component:(child_state, child_message) component)
    (instance:(state, message) instance)
    : child_state -> message Html.html
  =
    Log.warn(fun m->m"child invoked on instance");
    let child = child_instance ~message ?id component instance in
    view child


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
    fun state ->
      ChildCache.use cache (fun get_or_create ->
        state |> List.map (fun state ->
          let id = id state in
          let child_view = get_or_create id (fun _ ->
            let child: (child_state,message) instance = (child_instance
              ~message ~id component instance
            ) in
            view child
          ) in
          child_view state
        )
      )

  let collection ~id component instance = children ~id ~message:identity component instance

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
      let view_fn = view instance in
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
    init_logging ();
    (match (log, user_override_level) with
      | Some lvl, None -> set_log_level lvl
      | _ -> ()
    );
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
