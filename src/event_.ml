open Util_
open Event_types_
module Event = struct
  type 'msg result = 'msg Event_types_.result
  type event = Dom_html.event

  type response = [ | Event_types_.response ]

  type 'msg handler = event Js.t -> 'msg result

  let get_response x = x.response
  let get_message x = x.message

  let respond response = {
    response = response;
    message = None;
  }

  let handle msg = {
    response = `Handled;
    message = Some msg;
  }

  let return response msg = {
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

  let optional x = Option.default unhandled x

  let stop = {
    response = `Stop;
    message = None;
  }

  let lift (converter: event Js.t -> 'a Js.opt) ev fn =
    match (converter ev) |> Js.Opt.to_option with
    | Some x -> fn x
    | None -> unhandled

  let coerce coersion x = coersion x |> Js.Opt.to_option

  let mouse_event e = coerce Dom_html.CoerceTo.mouseEvent e
  let keyboard_event e = coerce Dom_html.CoerceTo.keyboardEvent e
  let target e = coerce (fun e -> e##.target) e
  let coerce_target coersion e = target e |> Option.bind (coerce coersion)

  let input_contents ev =
    coerce_target Dom_html.CoerceTo.input ev
      |> Option.map (fun input -> Js.to_string (input##.value))

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
