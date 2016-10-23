open Util_
let () =
  let log_level = try
    match Unix.getenv "VERBOSE" with "1" -> Some (Logs.Debug) | _ -> None
    with Not_found -> None
  in
  Ui.set_log_level (log_level |> Option.default Logs.Warning);
  Test_vdom.init ();
  Test_diff.init ();
  Test_ui.init ();
  Ppx_test.Test.collect ();
  ()
