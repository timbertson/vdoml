module Base_app = struct
  module Hooks = Diff_.No_hooks
end

module type S = App_.S

module Make(App:S) = struct
  module Html = Html_.Make(App)
  module Ui = Ui_.Make(App)
  module Curry = Curry
  module Collection_cache = Collection_cache
end
