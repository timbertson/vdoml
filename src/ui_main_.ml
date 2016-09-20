open Log_
module Ui_main = struct
  type context = {
    thread: unit Lwt.t;
    main: unit Lwt.t;
    enqueue: unit Lwt.t -> unit;
  }

  module Diff = Diff_.Make(Diff_.No_hooks)

  let init root =
    let queue, emit = Lwt_stream.create () in
    let enqueue x = emit (Some x) in
    let first_error = ref None in
    let main, wakener = Lwt.task () in
    let spawner = Lwt_stream.iter_p (fun task ->
      let task_thread =
        try%lwt
          task
        with
          | Lwt.Canceled -> Lwt.return_unit
          | e ->
            let () = match !first_error with
              | Some _ -> ()
              | None ->
                first_error := Some (e, Printexc.get_backtrace ());
                Lwt.wakeup_exn wakener e
            in
            Lwt.return_unit
      in
      let main_monitor =
        Lwt.finalize (fun () -> main) (fun () -> Lwt.cancel task; Lwt.return_unit);
      in
      Lwt.choose [ task_thread; main_monitor ]
    ) queue in
      
    let main =
      (* end the spawner loop when `main` finishes *)
      Lwt.finalize (fun () -> main) (fun () -> emit None; Lwt.return_unit)
    in

    let thread = Lwt.finalize (fun () -> Lwt.join [main; spawner]) (fun () ->
      match !first_error with
      | None ->
        Log.info (fun m->m "UI cancelled");
        Diff.remove_all_dom root;
        Lwt.return_unit
      | Some (e, backtrace) ->
        let err = Printexc.to_string e in
        Log.err (fun m -> m "%s\n%s" err backtrace);
        (try
          Diff.prepend ~emit:ignore (
            let open Html in
            div [
              h1 [ text "Uncaught error:" ];
              h2 [ text err ];
              pre [ text backtrace ];
              hr ();
            ]
          ) root;
          with _ -> ()
        );
        Lwt.fail e
    ) in
  { thread; main; enqueue }

  let wait ctx = ctx.thread
  let async ctx = ctx.enqueue
  let cancel ctx = Lwt.cancel ctx.main
end


