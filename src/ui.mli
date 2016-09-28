(** Top-level user-interface functionality *)

(** {2 UI Components}

  UI components consist of three main parts:
   - state: the current
   - view function: takes state and generates the current (virtual) HTML to represent this component
   - update function: takes the current state, an update message, and returns the new state. Also known as a step function,
     as it effectively implements a single "step" of the component's state machine.
   *)

type ('state, 'command, 'message) component
type ('state, 'command) instance
type ('state, 'command) view_fn = ('state, 'command) instance -> 'state -> 'command Html.html
type 'command emit_fn = 'command -> unit
type 'command node = 'command Html.html

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
  intercept:(('state, 'command) instance -> 'state -> 'command -> 'message option) ->
  update:('state -> 'message -> 'state) ->
  view:('state, 'command) view_fn ->
  'state -> ('state, 'command, 'message) component
(** Create a component with the given view, intercept, and update functions with the given initial state. *)

val pure_component :
  update:('state -> 'command -> 'state) ->
  view:('state, 'command) view_fn ->
  'state -> ('state, 'command, 'command) component
(** Create a component with the given view function, update function and initial state. *)

val emit : ('state, 'command) instance -> 'command emit_fn
(** Emit an update message to the given instance *)

(* like `children`, but with type 'command = `child_message` - i.e. no message conversion *)
val collection : view:('child_state, 'command) view_fn
  -> id:('child_state -> identity)
  -> ('state, 'command) instance
  -> 'child_state list
  -> 'command node list

val children : view:('child_state, 'child_command) view_fn
  -> message:('child_command -> 'command)
  -> id:('child_state -> identity)
  -> ('state, 'command) instance
  -> 'child_state list
  -> 'command node list

val child : view:('child_state, 'child_command) view_fn
  -> message:('child_command -> 'command)
  -> ?id:identity
  -> ('state, 'command) instance
  -> 'child_state
  -> 'command Html.html

val bind : ('state, 'command) instance
  -> ('state -> 'arg -> 'command Event.result)
  -> ('arg -> 'command Event.result)

module Tasks : sig
  type ('state, 'command) t
  val init : unit -> ('state, 'command) t
  val of_sync : (('state, 'command) instance -> unit) -> ('state, 'command) t
  val of_async : (('state, 'command) instance -> unit Lwt.t) -> ('state, 'command) t
  val sync : ('state, 'command) t -> (('state, 'command) instance -> unit) -> unit
  val async : ('state, 'command) t -> (('state, 'command) instance -> unit Lwt.t) -> unit
end

type context
val async : ('state, 'command) instance -> unit Lwt.t -> unit
val abort : ('state, 'command) instance -> unit
val wait : context -> unit Lwt.t

val onload : (unit -> unit Lwt.t) -> unit

val main :
  ?log:Logs.level
  -> ?root:string
  -> ?get_root:(unit -> Dom_html.element Js.t)
  -> ?tasks:('state, 'command) Tasks.t
  -> ('state, 'command, 'message) component
  -> unit -> unit Lwt.t

(** {2 Advanced API} *)

val render :
  ?tasks:('state, 'command) Tasks.t
  -> ('state, 'command, 'message) component
  -> Dom_html.element Js.t
  -> ('state, 'command) instance * context
  (** Begin a render lifecycle for a toplevel component.
      Returns the Ui instance corresponding to the component,
      as well as a {!context} representing the lifecycle
      of this component tree. *)

val init_logging: unit -> unit
(** ensure logging is initialized. Ui.main will call this for you *)

val set_log_level: Logs.level -> unit
(** ensure logging is initialized. Ui.main will call this for you *)

