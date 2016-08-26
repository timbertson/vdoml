open Vdom_
open Attr_


(* additional html utils not provided by tyxml *)
(* let a_on event (handler : (Dom_html.event) Js.t -> event_response) : Attr.optional = *)
(*   Attr.event_handler_attr ("on" ^ event) (handler:>(biggest_event Js.t -> event_response)) *)

let s_class c = Attr.attribute "class" c
let text = Vdom.text

module Input = struct
  type event = {
    event : Dom_html.event Js.t;
    element : Dom_html.inputElement Js.t;
    contents: string;
  }

  let event e = e.event
  let element e = e.element
  let contents e = e.contents

  let lift (handler: event -> event_response) = (fun ev ->
    let ev = (ev :> Dom_html.event Js.t) in
    let rv = ref None in
    Js.Opt.iter (ev##.target) (fun target ->
      Js.Opt.iter (Dom_html.CoerceTo.input target) (fun input ->
        let arg = {
          event = ev;
          element = input;
          contents = Js.to_string (input##.value);
        } in
        rv := Some (handler arg)
      )
    );
    match !rv with
      | Some rv -> rv
      | None -> `Unhandled
  )
end

include Html_gen_
