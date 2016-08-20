val init_logging: unit -> unit
val set_log_level: Logs.level -> unit

type ('elt, 'model, 'message) component
type ('elt, 'model, 'message) instance
type ('elt, 'model, 'message) view_fn = ('elt, 'model, 'message) instance -> 'model -> 'elt Html.elt
type 'message emit_fn = 'message -> unit
type identity = [ `String of string | `Int of int ]

type node = Html.vdom_node

val component : 
  update:('model -> 'message -> 'model) ->
  view:('elt, 'model, 'message) view_fn ->
  'model -> ('elt, 'model, 'message) component

val identify : identity -> node -> node
val emit : ('elt, 'model, 'message) instance -> 'message emit_fn

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

val render :
  ('elt, 'model, 'message) component
  -> Dom_html.element Js.t
  -> ('elt, 'model, 'message) instance * context

val onload : (unit -> unit Lwt.t) -> unit

val main :
  ?log:Logs.level
  -> ?root:string
  -> ?get_root:(unit -> Dom_html.element Js.t)
  -> ?background:(('elt, 'model, 'message) instance -> unit)
  -> ('elt, 'model, 'message) component
  -> unit -> unit Lwt.t
