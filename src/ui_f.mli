module type UI = Ui_.UI

module type DOM_HOOKS = Diff_.DOM_HOOKS

module No_hooks : DOM_HOOKS

module Make(Hooks:DOM_HOOKS) : UI
