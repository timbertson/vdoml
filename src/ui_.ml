open Log_
open Vdom_
open Attr_
open Ui_main_

let identity x = x

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

  type node = Html_.vdom_node


  type ('elt, 'a) instance_state = {
    state_val: 'a;
    state_counter: int;
    state_view: Vdom.node;
  }

  type 'message emit_fn = 'message -> unit

  and ('elt, 'model, 'message) view_fn = ('elt, 'model, 'message) instance -> 'model -> 'elt Html.elt

  and ('elt, 'model, 'message) component = {
    init: 'model;
    update: 'model -> 'message -> 'model;
    component_view: ('elt, 'model, 'message) view_fn;
  }

  and ('elt, 'model, 'message) instance = {
    context: context;
    emit: 'message emit_fn;
    identity: Vdom.identity;
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
    let open Vdom in
    function
      | Some id -> User_tag id
      | None ->
        let tag = !tag_counter in
        tag_counter := tag + 1;
        Internal_tag tag

  let wrap_message instance wrapper =
    (fun msg -> instance.emit (wrapper msg))

  let component
      ~(update:'model -> 'message -> 'model)
      ~(view: ('elt, 'model, 'message) view_fn)
      (init:'model) = 
    { init; update; component_view = view }

  let update_and_view instance =
      let id = instance.identity in
      let render = let view = instance.instance_view instance in
        fun state -> Vdom.identify_anonymous id (view state) in
    fun state ->
      (* Note: this is effectful because we need to store `state` separate from VDOM :( *)
      let state = match !(instance.state) with
        | None -> State.init (render state) state
        | Some existing -> State.update render existing state
      in
      instance.state := Some (state);
      state.state_view

  let instantiate ~context component =
    let events, emit = Lwt_stream.create () in
    let instance = {
      context;
      emit = (fun msg -> emit (Some msg));
      identity = gen_identity None;
      state = ref None;
      instance_view = component.component_view;
    } in
    (instance, events)

  let emit instance = instance.emit

  let bind instance handler = (fun evt ->
    (* XXX this relies on never rendering any instance
     * twice in the DOM tree. That seems like a good restriction,
     * but it's not enforced anywhere
     *)
    match !(instance.state) with
    | Some state -> handler state.state_val evt
    | None -> `Unhandled
  )

  let handler instance ?(response=`Handled) handler =
    let emit = emit instance in
    bind instance (fun state event ->
      emit (handler state event);
      response
    )

  let emitter instance ?(response=`Handled) msg =
    let emit = emit instance in
    (fun _evt ->
      emit msg;
      response
    )

  let handle ?(response=`Handled) handler = fun arg ->
    handler arg;
    response

  (* default implementation: just use a list *)
  module ChildCache = Collection_cache.Make(Collection_cache.Child_list)

  let children ~view ~message ~id instance =
    let open Vdom in
    let emit = wrap_message instance message in
    let cache = ChildCache.init () in
    fun state ->
      ChildCache.use cache (fun get_or_create ->
        state |> List.map (fun state ->
          let id = User_tag (id state) in
          let child = get_or_create id (fun _ -> update_and_view {
            context = instance.context;
            emit = emit;
            identity = id;
            instance_view = view;
            state = ref None;
          }) in
          child state
        )
      )

  let collection ~view ~id instance = children ~view ~id ~message:identity instance

  let child ~view ~message ?id instance =
    let child : ('elt, 'model, 'message) instance = {
      context = instance.context;
      emit = wrap_message instance message;
      identity = gen_identity id;
      instance_view = view;
      state = ref None;
    } in
    update_and_view child

  let async instance th = Ui_main.async (instance.context) th

  let render
      (component: ('elt, 'model, 'message) component)
      (root:Dom_html.element Js.t)
      : ('elt, 'model, 'message) instance * context =

    let context = Ui_main.init root in
    let (instance, events) = instantiate ~context component in
    async instance (
      let view_fn = update_and_view instance in
      let state = ref component.init in
      let view = ref (view_fn !state) in
      Diff.init !view root;
      Lwt_stream.iter (fun message ->
        let new_state = component.update !state message in
        let new_view = view_fn new_state in
        Log.info (fun m -> m "Updating view");
        Diff.update !view new_view root;
        state := new_state;
        view := new_view;
      ) events
    );
    (instance, context)

  let onload fn =
    let open Lwt in
    ignore_result (Lwt_js_events.onload () >>= (fun _evt -> fn ()))

  let main ?log ?root ?get_root ?background ui () =
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
            (fun () -> raise (Diff_.Assertion_error ("Element with id " ^ id ^ " not found")))
      )
    in
    let instance, main = render ui (get_root ()) in
    let () = match background with
      | Some fn -> fn instance
      | None -> ()
    in
    Ui_main.wait main
end
