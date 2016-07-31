include Sexplib.Conv
module PTest = Ppx_test.Test

let wrap_suite fn =
  let open Js in
  let open Unsafe in
  let jsdom = eval_string "require('jsdom')" in
  meth_call jsdom "env" [| inject (obj [|
    "url", inject (string "http://example.com");
    "scripts", inject (null);
    "done", inject (callback (fun err window ->
      set global "window" window;
      let cleanup () = set global "window" null in
      try
        fn ();
        cleanup ()
      with e -> (cleanup (); raise e)
    ))
  |])|]

