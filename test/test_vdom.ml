open Test_util

let %TEST one_is_two_ =
  [%test_eq:int] 1 1

let init () = ()
