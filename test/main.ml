let () =
  Test_vdom.init ();
  Test_ui.init ();
  Ppx_test.Test.collect ();
  ()
