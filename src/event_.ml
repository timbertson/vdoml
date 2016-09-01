open Event_types_
module Event = struct
  type 'msg result = 'msg Event_types_.result

  (* Why can't I alias Event_types_.* for these next two types? *)
  type response = [
    | `Unhandled
    | `Handled
    | `Stop
  ]
  class type biggest_event = object
    inherit Dom_html.event
    inherit Dom_html.mouseEvent
    inherit Dom_html.keyboardEvent
  end

  type 'msg handler = biggest_event Js.t -> 'msg result

  let response x = x.response
  let message x = x.message

  let return response = {
    response = response;
    message = None;
  }

  let emit ?(response=`Handled) msg = {
    response = response;
    message = Some msg;
  }

  let handled = {
    response = `Handled;
    message = None;
  }

  let unhandled = {
    response = `Unhandled;
    message = None;
  }

  let stop = {
    response = `Stop;
    message = None;
  }

  let lift (converter: biggest_event Js.t -> 'a Js.opt) ev fn =
    match (converter ev) |> Js.Opt.to_option with
    | Some x -> fn x
    | None -> unhandled

  let mouse_event ev = lift (Dom_html.CoerceTo.mouseEvent) ev
  let keyboard_event ev = lift (Dom_html.CoerceTo.keyboardEvent) ev
  let input_contents ev = lift (fun ev ->
    Js.Opt.map
      (Js.Opt.bind (ev##.target) (fun target -> Dom_html.CoerceTo.input target))
      (fun input -> Js.to_string (input##.value))
  ) ev


  let apply emit { response; message } =
    let () = match message with
      | Some msg -> emit msg
      | None -> ()
    in
    response

  let map_msg fn {message; response} =
    match message with
      | Some msg -> { response; message = Some (fn msg) }
      | None -> { response; message = None }
end
