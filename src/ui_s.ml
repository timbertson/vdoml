module type S = sig
  (** Top-level user-interface functionality *)

  (** {2 UI Components}

    UI components consist of three main parts:
     - state: the current
     - view function: takes state and generates the current (virtual) HTML to represent this component
     - update function: takes the current state, an update message, and returns the new state. Also known as a step function,
       as it effectively implements a single "step" of the component's state machine.
     *)

  type event_response = Html_.event_response
  type message
  type node
  type event_handler

  type ('elt, 'state, 'message) component
  type ('elt, 'state, 'message) instance
  type ('elt, 'state, 'message) view_fn = ('elt, 'state, 'message) instance -> 'state -> node
  type 'message emit_fn = 'message -> unit

  type identity = [ `String of string | `Int of int ]
  val identify : identity -> node -> node
  (** Assign an identity to a node. This will be used by the diffing algorithm to
      track potentially-reordered nodes across susequent renders. *)

  val component : 
    update:('state -> 'message -> 'state)
    -> view:('elt, 'state, 'message) view_fn
    -> unit
    -> ('elt, 'state, 'message) component
  (** Create a component with the given view function, update function and initial state. *)

  val emit : ('elt, 'state, 'message) instance -> 'message emit_fn
  (** Emit an update message to the given instance *)

  val send : ('elt, 'state, 'message) instance -> 'message -> event_handler

  (* like `children`, but with type 'message = `child_message` - i.e. no message conversion *)
  val collection : view:('child_elt, 'child_state, 'message) view_fn
    -> id:('child_state -> identity)
    -> ('elt, 'state, 'message) instance
    -> 'child_state list
    -> node list

  val children : view:('child_elt, 'child_state, 'child_message) view_fn
    -> message:('child_message -> 'message)
    -> id:('child_state -> identity)
    -> ('elt, 'state, 'message) instance
    -> 'child_state list
    -> node list

  val child : view:('child_elt, 'child_state, 'child_message) view_fn
    -> message:('child_message -> 'message)
    -> ?id:identity
    -> ('elt, 'state, 'message) instance
    -> 'child_state
    -> node

  val bind : ('elt, 'state, 'message) instance
    -> ('state -> 'arg -> event_response)
    -> ('arg -> event_response)

  val handler :
    ('elt, 'state, 'message) instance
    -> ?response:event_response
    -> ('state -> 'arg -> 'message)
    -> ('arg -> event_response)

  val emitter :
    ('elt, 'state, 'message) instance
    -> ?response:event_response
    -> 'message
    -> ('ignored -> event_response)

  val handle : ?response:event_response
    -> ('arg -> unit)
    -> ('arg -> event_response)

  type context
  val async : ('elt, 'state, 'message) instance -> unit Lwt.t -> unit
  val abort : ('elt, 'state, 'message) instance -> unit

  val onload : (unit -> unit Lwt.t) -> unit

  val main :
    ?log:Logs.level
    -> ?root:string
    -> ?get_root:(unit -> Dom_html.element Js.t)
    -> ?background:(('elt, 'state, message) instance -> unit)
    -> ('elt, 'state, message) component
    -> 'state
    -> unit -> unit Lwt.t

  (** {2 Advanced API} *)

  val render :
    ('elt, 'state, message) component
    -> 'state
    -> Dom_html.element Js.t
    -> ('elt, 'state, message) instance * context
    (** Begin a render lifecycle for a toplevel component.
        Returns the Ui instance corresponding to the component,
        as well as a {!context} representing the lifecycle
        of this component tree. *)

  val init_logging: unit -> unit
  (** ensure logging is initialized. Ui.main will call this for you *)

  val set_log_level: Logs.level -> unit
  (** ensure logging is initialized. Ui.main will call this for you *)
end
