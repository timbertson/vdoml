open Log_
module Ui_main = struct
  type context = {
    thread: unit Lwt.t;
    enqueue: unit Lwt.t -> unit;
  }

  module Diff = Diff_.Make(Diff_.No_hooks)

  let init root =
    let queue, enqueue = Lwt_stream.create () in
    let enqueue x = enqueue (Some x) in
    let thread = (
      try%lwt
        Lwt_stream.iter_p (fun result -> result) queue
      with e ->  (
        let backtrace = Printexc.get_backtrace () in
        let err = Printexc.to_string e in
        Log.err (fun m -> m "%s\n%s" err backtrace);
        (try
          (* Diff.remove_all root; *)
          Diff.prepend (
            let open Html in
            div [
              h1 [ pcdata "Uncaught error:" ];
              h2 [ pcdata err ];
              pre [ pcdata backtrace ];
              hr ();
            ]
          ) root;
          with _ -> ()
        );
        raise e
      )
    ) in
  { thread; enqueue }

  let wait ctx = ctx.thread
  let async ctx = ctx.enqueue
end

