open Vdoml
open Util_
let () =
  let log_level = try
    match Unix.getenv "VERBOSE" with "1" -> Some (Logs.Debug) | _ -> None
    with Not_found -> None
  in
  Ui.set_log_level (log_level |> Option.default Logs.Warning);
  Sys.argv |> Array.to_list |> List.iter (fun arg ->
    print_endline("ARG: " ^ arg)
  );
  Test_vdom.init ();
  Test_diff.init ();
  Test_ui.init ();

  (* Base.Random whinges if you use random numbers in tests. I don't know who is causing it, and I don't care *)
  Base.Random.set_state (Base.Random.State.make_self_init ~allow_in_tests:true ());

  let open Ppx_inline_test_lib in

  print_endline (Ppx_inline_test_lib.Runtime.am_running_inline_test_env_var);
  print_endline (string_of_bool (Ppx_inline_test_lib.Runtime.am_running_inline_test));
  print_endline (string_of_bool (Base.Exported_for_specific_uses.am_testing));
  let () = (match Array.to_list Sys.argv with
    | name :: "inline-test-runner" :: lib :: rest ->
      print_endline ("tests! " ^ lib)
    | _ -> print_endline "no tests for you!"
  ) in
  Ppx_inline_test_lib.Runtime.exit ()
  (* () *)
