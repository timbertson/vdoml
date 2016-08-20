module type UI = module type of Ui

module type DOM_HOOKS = sig
  val register_element : Dom_html.element Js.t -> unit
  val unregister_element : Dom_html.element Js.t -> unit
end

module No_hooks : DOM_HOOKS

module Make(Hooks:DOM_HOOKS) : UI
