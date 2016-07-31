let () =
  Test_util.wrap_suite (fun () ->
    Test_vdom.init ();
    Ppx_test.Test.collect ();
  )
