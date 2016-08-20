(** Top-level user-interface functionality *)

(** {2 UI Components}

  UI components consist of three main parts:
   - state: the current
   - view function: takes state and generates the current (virtual) HTML to represent this component
   - update function: takes the current state, an update message, and returns the new state. Also known as a step function,
     as it effectively implements a single "step" of the component's state machine.
   *)

type ('elt, 'model, 'message) component
type ('elt, 'model, 'message) instance
type ('elt, 'model, 'message) view_fn = ('elt, 'model, 'message) instance -> 'model -> 'elt Html.elt
type 'message emit_fn = 'message -> unit
type node = Html.vdom_node

type identity = [ `String of string | `Int of int ]
val identify : identity -> node -> node
(** Assign an identity to a node. This will be used by the diffing algorithm to
    track potentially-reordered nodes across susequent renders. *)

val component : 
  update:('model -> 'message -> 'model) ->
  view:('elt, 'model, 'message) view_fn ->
  'model -> ('elt, 'model, 'message) component
(** Create a component with the given view function, update function and initial state. *)

val emit : ('elt, 'model, 'message) instance -> 'message emit_fn
(** Emit an update message to the given instance *)

(* like `children`, but with type 'message = `child_message` - i.e. no message conversion *)
val collection : view:('child_elt, 'child_model, 'message) view_fn
  -> id:('child_model -> identity)
  -> ('elt, 'model, 'message) instance
  -> 'child_model list
  -> node list

val children : view:('child_elt, 'child_model, 'child_message) view_fn
  -> message:('child_message -> 'message)
  -> id:('child_model -> identity)
  -> ('elt, 'model, 'message) instance
  -> 'child_model list
  -> node list

val child : view:('child_elt, 'child_model, 'child_message) view_fn
  -> message:('child_message -> 'message)
  -> ?id:identity
  -> ('elt, 'model, 'message) instance
  -> 'child_model
  -> node

val bind : ('elt, 'model, 'message) instance
  -> ('model -> 'arg -> Html.event_response)
  -> ('arg -> Html.event_response)

val handler :
  ('elt, 'model, 'message) instance
  -> ?response:Html.event_response
  -> ('model -> 'arg -> 'message)
  -> ('arg -> Html.event_response)

val emitter :
  ('elt, 'model, 'message) instance
  -> ?response:Html.event_response
  -> 'message
  -> ('ignored -> Html.event_response)

val handle : ?response:Html.event_response
  -> ('arg -> unit)
  -> ('arg -> Html.event_response)

type context
val async : ('elt, 'model, 'message) instance -> unit Lwt.t -> unit
val abort : ('elt, 'model, 'message) instance -> unit

val onload : (unit -> unit Lwt.t) -> unit

val main :
  ?log:Logs.level
  -> ?root:string
  -> ?get_root:(unit -> Dom_html.element Js.t)
  -> ?background:(('elt, 'model, 'message) instance -> unit)
  -> ('elt, 'model, 'message) component
  -> unit -> unit Lwt.t

(** {2 Advanced API} *)

val render :
  ('elt, 'model, 'message) component
  -> Dom_html.element Js.t
  -> ('elt, 'model, 'message) instance * context
  (** Begin a render lifecycle for a toplevel component.
      Returns the Ui instance corresponding to the component,
      as well as a {!context} representing the lifecycle
      of this component tree. *)

val init_logging: unit -> unit
(** ensure logging is initialized. Ui.main will call this for you *)

val set_log_level: Logs.level -> unit
(** ensure logging is initialized. Ui.main will call this for you *)

