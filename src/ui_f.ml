module type UI = Ui_.UI
module type DOM_HOOKS = Diff_.DOM_HOOKS
module Make = Ui_.Make
module No_hooks = Diff_.No_hooks
