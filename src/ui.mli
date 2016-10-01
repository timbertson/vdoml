(** Top-level user-interface functionality *)

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
type ('state, 'message) command_fn = 'message -> unit Lwt.t option
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
  ?command:(('state, 'message) instance -> ('state, 'message) command_fn) ->
  unit -> ('state, 'message) component

val root_component :
  update:('state -> 'message -> 'state) ->
  view:(('state, 'message) instance -> ('state, 'message) view_fn) ->
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
  -> 'child_state
  -> 'message Html.html

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

val supplantable : ('a -> 'message option Lwt.t)
  -> ('state, 'message) instance
  -> ('a -> unit Lwt.t)

val supplantable_some : ('a -> 'message Lwt.t)
  -> ('state, 'message) instance
  -> ('a -> unit Lwt.t)

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

