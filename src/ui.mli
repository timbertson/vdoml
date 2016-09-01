(** Top-level user-interface functionality *)

(** {2 UI Components}

  UI components consist of three main parts:
   - state: the current
   - view function: takes state and generates the current (virtual) HTML to represent this component
   - update function: takes the current state, an update message, and returns the new state. Also known as a step function,
     as it effectively implements a single "step" of the component's state machine.
   *)

type ('state, 'message) component
type ('state, 'message) instance
type ('state, 'message) view_fn = ('state, 'message) instance -> 'state -> 'message Html.html
type 'message emit_fn = 'message -> unit
type 'message node = 'message Html.html

type identity = [ `String of string | `Int of int ]
val identify : identity -> 'msg node -> 'msg node
(** Assign an identity to a node. This will be used by the diffing algorithm to
    track potentially-reordered nodes across susequent renders. *)

val component : 
  update:('state -> 'message -> 'state) ->
  view:('state, 'message) view_fn ->
  'state -> ('state, 'message) component
(** Create a component with the given view function, update function and initial state. *)

val emit : ('state, 'message) instance -> 'message emit_fn
(** Emit an update message to the given instance *)

(* like `children`, but with type 'message = `child_message` - i.e. no message conversion *)
val collection : view:('child_state, 'message) view_fn
  -> id:('child_state -> identity)
  -> ('state, 'message) instance
  -> 'child_state list
  -> 'message node list

val children : view:('child_state, 'child_message) view_fn
  -> message:('child_message -> 'message)
  -> id:('child_state -> identity)
  -> ('state, 'message) instance
  -> 'child_state list
  -> 'message node list

val child : view:('child_state, 'child_message) view_fn
  -> message:('child_message -> 'message)
  -> ?id:identity
  -> ('state, 'message) instance
  -> 'child_state
  -> 'message Html.html

val bind : ('state, 'message) instance
  -> ('state -> 'arg -> 'msg Event.result)
  -> ('arg -> 'msg Event.result)

type context
val async : ('state, 'message) instance -> unit Lwt.t -> unit
val abort : ('state, 'message) instance -> unit

val onload : (unit -> unit Lwt.t) -> unit

val main :
  ?log:Logs.level
  -> ?root:string
  -> ?get_root:(unit -> Dom_html.element Js.t)
  -> ?background:(('state, 'message) instance -> unit)
  -> ('state, 'message) component
  -> unit -> unit Lwt.t

(** {2 Advanced API} *)

val render :
  ('state, 'message) component
  -> Dom_html.element Js.t
  -> ('state, 'message) instance * context
  (** Begin a render lifecycle for a toplevel component.
      Returns the Ui instance corresponding to the component,
      as well as a {!context} representing the lifecycle
      of this component tree. *)

val init_logging: unit -> unit
(** ensure logging is initialized. Ui.main will call this for you *)

val set_log_level: Logs.level -> unit
(** ensure logging is initialized. Ui.main will call this for you *)

