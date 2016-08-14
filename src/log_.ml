let log_src = Logs.Src.create "vdom" ~doc:"vdom debugging"
module Log = (val Logs.src_log log_src : Logs.LOG)

