include Html_
open Vdom_
open Attr_

type 'msg html = 'msg Vdom.html
type 'msg attr = 'msg Attr.t
type event_response = Attr_.event_response
type 'msg event_handler = 'msg Attr.property

class type biggest_event = object
  inherit Dom_html.event
  inherit Dom_html.mouseEvent
  inherit Dom_html.keyboardEvent
end

let attr name value =
  match name with
  | "value" | "checked" | "selected" ->
    Attr.string_property name value
  | name ->
    Attr.attribute name value

let property = Attr.property

let entity e =
  let entity = Dom_html.decode_html_entities (Js.string ("&" ^ e ^ ";")) in
  Vdom.text (Js.to_string entity)

let leaf = Vdom.create_leaf
let node = Vdom.create

let a_on event handler =
  Attr.property ("on" ^ event) handler

let s_class c = Attr.attribute "class" c
let text = Vdom.text

(* --------- *)

let float_attrib name value : 'msg attr = attr name (string_of_float value)
let int_attrib name value = attr name (string_of_int value)
let string_attrib name value = attr name value
let uri_attrib = string_attrib
let space_sep_attrib name values = attr name (String.concat " " values)
let comma_sep_attrib name values = attr name (String.concat "," values)
let user_attrib f name v = string_attrib name (f v)

let bool_attrib = user_attrib string_of_bool

let string_of_character = String.make 1
let onoff_of_bool = function
  | false -> "off"
  | true -> "on"

type input_type = [
  | `Url
  | `Tel
  | `Text
  | `Time
  | `Search
  | `Password
  | `Checkbox
  | `Range
  | `Radio
  | `Submit
  | `Reset
  | `Number
  | `Hidden
  | `Month
  | `Week
  | `File
  | `Email
  | `Image
  | `Datetime_local
  | `Datetime
  | `Date
  | `Color
  | `Button
]

let string_of_numbers l =
    String.concat "," (List.map string_of_int l)

let constant_attrib a () =
  string_attrib a a

let string_of_input_type = function
  | `Button -> "button"
  | `Checkbox -> "checkbox"
  | `Color -> "color"
  | `Date -> "date"
  | `Datetime -> "datetime"
  | `Datetime_local -> "datetime-local"
  | `Email -> "email"
  | `File -> "file"
  | `Hidden -> "hidden"
  | `Image -> "image"
  | `Month -> "month"
  | `Number -> "number"
  | `Password -> "password"
  | `Radio -> "radio"
  | `Range -> "range"
  | `Readonly -> "readonly"
  | `Reset -> "reset"
  | `Search -> "search"
  | `Submit -> "submit"
  | `Tel -> "tel"
  | `Text -> "text"
  | `Time -> "time"
  | `Url -> "url"
  | `Week -> "week"

(* Core: *)
let a_class = string_attrib "class"
let a_class_list = space_sep_attrib "class"

let a_id = string_attrib "id"

let a_user_data name = string_attrib ("data-" ^ name)

let a_title = string_attrib "title"

(* I18N: *)
let a_xml_lang = string_attrib "xml:lang"
let a_lang = string_attrib "lang"

(* Style: *)
let a_style = string_attrib "style"

let a_property = string_attrib "property"

(* event attribute modifiers *)
let emit ?response e = Attr.message_emitter ?response e
let emitter ?response fn = Attr.message_fn ?response fn
let event_handler fn = Attr.message_response_fn fn

(* Events: *)
let a_onabort x = property "onabort" x
let a_onafterprint x = property "onafterprint" x
let a_onbeforeprint x = property "onbeforeprint" x
let a_onbeforeunload x = property "onbeforeunload" x
let a_onblur x = property "onblur" x
let a_oncanplay x = property "oncanplay" x
let a_oncanplaythrough x = property "oncanplaythrough" x
let a_onchange x = property "onchange" x
let a_ondurationchange x = property "ondurationchange" x
let a_onemptied x = property "onemptied" x
let a_onended x = property "onended" x
let a_onerror x = property "onerror" x
let a_onfocus x = property "onfocus" x
let a_onformchange x = property "onformchange" x
let a_onforminput x = property "onforminput" x
let a_onhashchange x = property "onhashchange" x
let a_oninput x = property "oninput" x
let a_oninvalid x = property "oninvalid" x
let a_onoffline x = property "onoffline" x
let a_ononline x = property "ononline" x
let a_onpause x = property "onpause" x
let a_onplay x = property "onplay" x
let a_onplaying x = property "onplaying" x
let a_onpagehide x = property "onpagehide" x
let a_onpageshow x = property "onpageshow" x
let a_onpopstate x = property "onpopstate" x
let a_onprogress x = property "onprogress" x
let a_onratechange x = property "onratechange" x
let a_onreadystatechange x = property "onreadystatechange" x
let a_onredo x = property "onredo" x
let a_onresize x = property "onresize" x
let a_onscroll x = property "onscroll" x
let a_onseeked x = property "onseeked" x
let a_onseeking x = property "onseeking" x
let a_onselect x = property "onselect" x
let a_onshow x = property "onshow" x
let a_onstalled x = property "onstalled" x
let a_onstorage x = property "onstorage" x
let a_onsubmit x = property "onsubmit" x
let a_onsuspend x = property "onsuspend" x
let a_ontimeupdate x = property "ontimeupdate" x
let a_onundo x = property "onundo" x
let a_onunload x = property "onunload" x
let a_onvolumechange x = property "onvolumechange" x
let a_onwaiting x = property "onwaiting" x
let a_onload x = property "onload" x
let a_onloadeddata x = property "onloadeddata" x
let a_onloadedmetadata x = property "onloadedmetadata" x
let a_onloadstart x = property "onloadstart" x
let a_onmessage x = property "onmessage" x
let a_onmousewheel x = property "onmousewheel" x

(** Javascript mouse events *)
let a_onclick x = property "onclick" x
let a_oncontextmenu x = property "oncontextmenu" x
let a_ondblclick x = property "ondblclick" x
let a_ondrag x = property "ondrag" x
let a_ondragend x = property "ondragend" x
let a_ondragenter x = property "ondragenter" x
let a_ondragleave x = property "ondragleave" x
let a_ondragover x = property "ondragover" x
let a_ondragstart x = property "ondragstart" x
let a_ondrop x = property "ondrop" x
let a_onmousedown x = property "onmousedown" x
let a_onmouseup x = property "onmouseup" x
let a_onmouseover x = property "onmouseover" x
let a_onmousemove x = property "onmousemove" x
let a_onmouseout x = property "onmouseout" x

(** Javascript keyboard events *)
let a_onkeypress x = property "onkeypress" x
let a_onkeydown x = property "onkeydown" x
let a_onkeyup x = property "onkeyup" x

(* Other Attributes *)
let a_version = string_attrib "version"

let a_xmlns x = string_attrib "xmlns" x

let a_manifest = string_attrib "manifest"

let a_cite = string_attrib "cite"

let a_xml_space = string_attrib "xml:space"

let a_accesskey c = user_attrib string_of_character "accesskey" c

let a_charset = string_attrib "charset"

let a_accept_charset = space_sep_attrib "accept-charset"

let a_accept = comma_sep_attrib "accept"

let a_href = string_attrib "href"

let a_hreflang = string_attrib "hreflang"

let a_download = string_attrib "download"

let a_rel = string_attrib "rel"

let a_tabindex = int_attrib "tabindex"

let a_mime_type = string_attrib "type"

let a_alt = string_attrib "alt"

let a_height p = int_attrib "height" p

let a_src = uri_attrib "src"

let a_width p = int_attrib "width" p

let a_label_for = string_attrib "for"
let a_for = a_label_for

let a_output_for = space_sep_attrib "for"
let a_for_list = a_output_for

let a_selected =
  constant_attrib "selected"

let a_text_value = string_attrib "value"

let a_int_value = int_attrib "value"

let a_value = string_attrib "value"

let a_float_value = float_attrib "value"

let a_action = uri_attrib "action"

let a_method = string_attrib "method"

let a_formmethod = a_method

let a_enctype = string_attrib "enctype"

let a_checked =
  constant_attrib "checked"

let a_disabled =
  constant_attrib "disabled"

let a_readonly =
  constant_attrib "readonly"

let a_maxlength = int_attrib "maxlength"

let a_name = string_attrib "name"

let a_autocomplete x =
  user_attrib onoff_of_bool "autocomplete" x

let a_async =
  constant_attrib "async"

let a_autofocus =
  constant_attrib "autofocus"

let a_autoplay =
  constant_attrib "autoplay"

let a_muted =
  constant_attrib "muted"

let a_crossorigin = string_attrib "crossorigin"

let a_mediagroup = string_attrib "mediagroup"

let a_challenge = string_attrib "challenge"

let a_contenteditable ce =
  bool_attrib "contenteditable" ce

let a_contextmenu = string_attrib "contextmenu"

let a_controls =
  constant_attrib "controls"

let a_dir = string_attrib "dir"

let a_draggable d =
  bool_attrib "draggable" d

let a_form = string_attrib "form"

let a_formaction = uri_attrib "formaction"

let a_formenctype = string_attrib "formenctype"

let a_formnovalidate =
  constant_attrib "formnovalidate"

let a_formtarget = string_attrib "formtarget"

let a_hidden =
  constant_attrib "hidden"

let a_high = float_attrib "high"

let a_icon = uri_attrib "icon"

let a_ismap =
  constant_attrib "ismap"

let a_keytype = string_attrib "keytype"

let a_list = string_attrib "list"

let a_loop =
  constant_attrib "loop"

let a_low = float_attrib "low"

let a_max = float_attrib "max"

let a_input_max = int_attrib "max"

let a_min = float_attrib "min"

let a_input_min = int_attrib "min"

let a_inputmode = string_attrib "inputmode"

let a_novalidate =
  constant_attrib "novalidate"

let a_open =
  constant_attrib "open"

let a_optimum = float_attrib "optimum"

let a_pattern = string_attrib "pattern"

let a_placeholder = string_attrib "placeholder"

let a_poster = uri_attrib "poster"

let a_preload = string_attrib "preload"

let a_pubdate =
  constant_attrib "pubdate"

let a_radiogroup = string_attrib "radiogroup"

let a_required =
  constant_attrib "required"

let a_reversed =
  constant_attrib "reserved"

let a_sandbox = string_attrib "sandbox"

let a_spellcheck sc =
  bool_attrib "spellcheck" sc

let a_scoped =
  constant_attrib "scoped"

let a_seamless =
  constant_attrib "seamless"

let a_sizes = string_attrib "sizes"

let a_span = int_attrib "span"

let a_srcset = string_attrib "srcset"

let a_img_sizes = comma_sep_attrib "sizes"

let a_start = int_attrib "start"

let a_step = string_attrib "step"

let a_wrap = string_attrib "wrap"

let a_size = int_attrib "size"

let a_input_type it = user_attrib string_of_input_type "type" it

let a_menu_type = string_attrib "type"

let a_command_type = string_attrib "type"

let a_button_type bt = user_attrib string_of_input_type "type" bt

let a_multiple = constant_attrib "multiple"

let a_cols = int_attrib "cols"

let a_rows = int_attrib "rows"

let a_summary = string_attrib "summary"

let a_align = string_attrib "align"

let a_axis = string_attrib "axis"

let a_colspan = int_attrib "colspan"

let a_headers = space_sep_attrib "headers"

let a_rowspan = int_attrib "rowspan"

let a_scope = string_attrib "scope"

let a_border = int_attrib "border"

let a_rules = string_attrib "rules"

let a_char c = user_attrib string_of_character "char" c

let a_data = uri_attrib "data"

let a_codetype = string_attrib "codetype"

let a_frameborder = string_attrib "frameborder"

let a_marginheight = int_attrib "marginheight"

let a_marginwidth = int_attrib "marginwidth"

let a_scrolling = string_attrib "scrolling"

let a_target = string_attrib "target"

let a_content = string_attrib "content"

let a_http_equiv = string_attrib "http-equiv"

let a_media = string_attrib "media"

(* Elements *)

(** A nullary element is an element that doesn't have any children. *)
type 'a nullary = ?a:('a attr list) -> unit -> 'a html

(** A unary element is an element that have exactly one children. *)
type 'a unary = ?a:('a attr list) -> 'a html -> 'a html

(** A star element is an element that has any number of children, including zero. *)
type 'a star =
  ?a:('a attr list) -> 'a html list -> 'a html


let terminal tag ?a () = leaf ?a tag

let unary tag ?a elt = node ?a tag [elt]

let star tag ?a elts = node ?a tag elts

let body ?a x = star "body" ?a x

let head ?a x = star "head" ?a x

let title ?a x = unary "title" ?a x

let html ?a x = star "html" ?a x

let footer ?a x = star "footer" ?a x

let header ?a x = star "header" ?a x

let section ?a x = star "section" ?a x

let nav ?a x = star "nav" ?a x

let entity = entity

let space () = entity "nbsp"

let h1 ?a x = star "h1" ?a x

let h2 ?a x = star "h2" ?a x

let h3 ?a x = star "h3" ?a x

let h4 ?a x = star "h4" ?a x

let h5 ?a x = star "h5" ?a x

let h6 ?a x = star "h6" ?a x

let hgroup ?a x = star "hgroup" ?a x

let address ?a x = star "address" ?a x

let blockquote ?a x = star "blockquote" ?a x

let div ?a x = star "div" ?a x

let p ?a x = star "p" ?a x

let pre ?a x = star "pre" ?a x

let abbr ?a x = star "abbr" ?a x

let br ?a x = terminal "br" ?a x

let cite ?a x = star "cite" ?a x

let code ?a x = star "code" ?a x

let dfn ?a x = star "dfn" ?a x

let em ?a x = star "em" ?a x

let kbd ?a x = star "kbd" ?a x

let q ?a x = star "q" ?a x

let samp ?a x = star "samp" ?a x

let span ?a x = star "span" ?a x

let strong ?a x = star "strong" ?a x

let time ?a x = star "time" ?a x

let var ?a x = star "var" ?a x

let a ?a x = star "a" ?a x

let dl ?a x = star "dl" ?a x

let ol ?a x = star "ol" ?a x

let ul ?a x = star "ul" ?a x

let dd ?a x = star "dd" ?a x

let dt ?a x = star "dt" ?a x

let li ?a x = star "li" ?a x

let hr ?a x = terminal "hr" ?a x

let b ?a x = star "b" ?a x

let i ?a x = star "i" ?a x

let u ?a x = star "u" ?a x

let small ?a x = star "small" ?a x

let sub ?a x = star "sub" ?a x

let sup ?a x = star "sup" ?a x

let mark ?a x = star "mark" ?a x

let rp ?a x = star "rp" ?a x

let rt ?a x = star "rt" ?a x

let ruby ?a x = star "ruby" ?a x

let wbr ?a x = terminal "wbr" ?a x

(* VB *)
let bdo ?a x = star "bdo" ?a x

let a_datetime = string_attrib "datetime"

let a_shape = string_attrib "shape"

let a_coords coords = user_attrib string_of_numbers "coords" coords

let a_usemap = string_attrib "usemap"

let a_defer =
  constant_attrib "defer"

let a_label = string_attrib "label"

let area ~alt ?(a = []) () = leaf ~a: ((a_alt alt) :: a) "area"

let map ?a x = star "map" ?a x

let del ?a x = star "del" ?a x

let ins ?a x = star "ins" ?a x

let script ?a x = unary "script" ?a x

let noscript ?a x = star "noscript" ?a x

let article ?a x = star "article" ?a x

let aside ?a x = star "aside" ?a x

let main ?a x = star "main" ?a x

let audio ?a x = star "audio" ?a x

let video ?a x = star "video" ?a x

let canvas ?a x = star "canvas" ?a x

let menu ?a x = star "menu" ?a x

let embed ?a x = terminal "embed" ?a x

let source ?a x = terminal "source" ?a x

let meter ?a x = star "meter" ?a x

let output_elt ?a x = star "output" ?a x

let form ?a x = star "form" ?a x

let svg ?a x = star "svg" ?a x

let input ?a x = terminal "input" ?a x

let keygen ?a x = terminal "keygen" ?a x

let label ?a x = star "label" ?a x

let option ?a x = unary "option" ?a x

let select ?a x = star "select" ?a x

let textarea ?a x = unary "textarea" ?a x

let button ?a x = star "button" ?a x

let datalist ?a x = star "datalist" ?a x

let progress ?a x = star "progress" ?a x

let legend ?a x = star "legend" ?a x

let details ?a x = star "details" ?a x

let summary ?a x = star "summary" ?a x

let fieldset ?a x = star "fieldset" ?a x

let optgroup ?a x = star "optgroup" ?a x

let figcaption ?a x = star "figcaption" ?a x

let figure ?a x = star "figure" ?a x

let caption ?a x = star "caption" ?a x

let table ?a x = star "table" ?a x

let td ?a x = star "td" ?a x

let th ?a x = star "th" ?a x

let tr ?a x = star "tr" ?a x

let colgroup ?a x = star "colgroup" ?a x

let col ?a x = terminal "col" ?a x

let thead ?a x = star "thead" ?a x

let tbody ?a x = star "tbody" ?a x

let tfoot ?a x = star "tfoot" ?a x

let iframe ?a x = star "iframe" ?a x

let object_ ?a x = star "object" ?a x

let param ?a x = terminal "param" ?a x

let img ~src ?(a = []) () =
  let a = (a_src src) :: a in
  leaf ~a "img"

let meta ?a x = terminal "meta" ?a x

let style ?a x = unary "style" ?a x

let link ~href ?(a = []) () =
  leaf ~a: ((a_href href) :: a) "link"

let base ?a () = terminal "base" ?a ()
