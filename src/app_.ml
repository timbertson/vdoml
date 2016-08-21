module type DOM_HOOKS = sig
  val register_element : Dom_html.element Js.t -> unit
  val unregister_element : Dom_html.element Js.t -> unit
end

module type S = sig
  type message
  module Hooks : DOM_HOOKS
end
