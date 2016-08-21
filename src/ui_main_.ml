open Log_
module Ui_main = struct
  type context = {
    thread: unit Lwt.t;
    enqueue: unit Lwt.t -> unit;
  }

  module Noop_app = struct
    type message = unit
    module Hooks : App_.DOM_HOOKS = Diff_.No_hooks
  end

  module Diff = Diff_.Make(Noop_app)
  module Html = Html_.Make(Noop_app)

  let init root =
    let queue, enqueue = Lwt_stream.create () in
    let enqueue x = enqueue (Some x) in
    let thread = (
      try%lwt
        Lwt_stream.iter_p (fun result -> result) queue
      with
      | Lwt.Canceled -> (
        Log.info (fun m->m "UI cancelled");
        Diff.remove_all root;
        Lwt.return_unit
      )
      | e -> (
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
  let cancel ctx = Lwt.cancel ctx.thread
end


