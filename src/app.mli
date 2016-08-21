module type S = App_.S

module Make : functor (App:S) -> sig
  module Html : Html_s.S with type message = App.message
  module Ui : Ui_s.S with type message = App.message and type node = Html.vdom_node
end
