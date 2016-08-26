(* Based on TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2004 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
 * Copyright (C) 2007 by Vincent Balat, Gabriel Kerneis
 * Copyright (C) 2010 by Cecile Herbelin
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02111-1307, USA.
*)

open Vdom_
open Attr_
module Xml = struct
  type 'msg html = 'msg Vdom.html
  type 'msg attr = 'msg Attr.optional
  type event_response = Attr_.event_response
  type event_handler = Dom_html.event Js.t -> event_response
  type mouse_event_handler = Dom_html.mouseEvent Js.t -> event_response
  type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> event_response

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
  let text = Vdom.text
end

include Xml

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
let emit ?(response=`Handled) e = Attr.Message_emitter (e, response)
let emitter fn = Attr.Message_fn fn
let emitter_response fn = Attr.Message_response_fn fn

(* Events: *)
let a_onabort = Xml.property "onabort"
let a_onafterprint = Xml.property "onafterprint"
let a_onbeforeprint = Xml.property "onbeforeprint"
let a_onbeforeunload = Xml.property "onbeforeunload"
let a_onblur = Xml.property "onblur"
let a_oncanplay = Xml.property "oncanplay"
let a_oncanplaythrough = Xml.property "oncanplaythrough"
let a_onchange = Xml.property "onchange"
let a_ondurationchange = Xml.property "ondurationchange"
let a_onemptied = Xml.property "onemptied"
let a_onended = Xml.property "onended"
let a_onerror = Xml.property "onerror"
let a_onfocus = Xml.property "onfocus"
let a_onformchange = Xml.property "onformchange"
let a_onforminput = Xml.property "onforminput"
let a_onhashchange = Xml.property "onhashchange"
let a_oninput = Xml.property "oninput"
let a_oninvalid = Xml.property "oninvalid"
let a_onoffline = Xml.property "onoffline"
let a_ononline = Xml.property "ononline"
let a_onpause = Xml.property "onpause"
let a_onplay = Xml.property "onplay"
let a_onplaying = Xml.property "onplaying"
let a_onpagehide = Xml.property "onpagehide"
let a_onpageshow = Xml.property "onpageshow"
let a_onpopstate = Xml.property "onpopstate"
let a_onprogress = Xml.property "onprogress"
let a_onratechange = Xml.property "onratechange"
let a_onreadystatechange = Xml.property "onreadystatechange"
let a_onredo = Xml.property "onredo"
let a_onresize = Xml.property "onresize"
let a_onscroll = Xml.property "onscroll"
let a_onseeked = Xml.property "onseeked"
let a_onseeking = Xml.property "onseeking"
let a_onselect = Xml.property "onselect"
let a_onshow = Xml.property "onshow"
let a_onstalled = Xml.property "onstalled"
let a_onstorage = Xml.property "onstorage"
let a_onsubmit = Xml.property "onsubmit"
let a_onsuspend = Xml.property "onsuspend"
let a_ontimeupdate = Xml.property "ontimeupdate"
let a_onundo = Xml.property "onundo"
let a_onunload = Xml.property "onunload"
let a_onvolumechange = Xml.property "onvolumechange"
let a_onwaiting = Xml.property "onwaiting"
let a_onload = Xml.property "onload"
let a_onloadeddata = Xml.property "onloadeddata"
let a_onloadedmetadata = Xml.property "onloadedmetadata"
let a_onloadstart = Xml.property "onloadstart"
let a_onmessage = Xml.property "onmessage"
let a_onmousewheel = Xml.property "onmousewheel"

(** Javascript mouse events *)
let a_onclick = Xml.property "onclick"
let a_oncontextmenu = Xml.property "oncontextmenu"
let a_ondblclick = Xml.property "ondblclick"
let a_ondrag = Xml.property "ondrag"
let a_ondragend = Xml.property "ondragend"
let a_ondragenter = Xml.property "ondragenter"
let a_ondragleave = Xml.property "ondragleave"
let a_ondragover = Xml.property "ondragover"
let a_ondragstart = Xml.property "ondragstart"
let a_ondrop = Xml.property "ondrop"
let a_onmousedown = Xml.property "onmousedown"
let a_onmouseup = Xml.property "onmouseup"
let a_onmouseover = Xml.property "onmouseover"
let a_onmousemove = Xml.property "onmousemove"
let a_onmouseout = Xml.property "onmouseout"

(** Javascript keyboard events *)
let a_onkeypress = Xml.property "onkeypress"
let a_onkeydown = Xml.property "onkeydown"
let a_onkeyup = Xml.property "onkeyup"

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

(*let a_srcdoc*)
let a_srclang = string_attrib "xml:lang"

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

let terminal tag ?a () = Xml.leaf ?a tag

let unary tag ?a elt = Xml.node ?a tag [elt]

let star tag ?a elts = Xml.node ?a tag elts

let body = star "body"

let head = star "head"

let title = unary "title"

let html = star "html"

let footer = star "footer"

let header = star "header"

let section = star "section"

let nav = star "nav"

let entity = Xml.entity

let space () = entity "nbsp"

let h1 = star "h1"

let h2 = star "h2"

let h3 = star "h3"

let h4 = star "h4"

let h5 = star "h5"

let h6 = star "h6"

let hgroup = star "hgroup"

let address = star "address"

let blockquote = star "blockquote"

let div = star "div"

let p = star "p"

let pre = star "pre"

let abbr = star "abbr"

let br = terminal "br"

let cite = star "cite"

let code = star "code"

let dfn = star "dfn"

let em = star "em"

let kbd = star "kbd"

let q = star "q"

let samp = star "samp"

let span = star "span"

let strong = star "strong"

let time = star "time"

let var = star "var"

let a = star "a"

let dl = star "dl"

let ol = star "ol"

let ul = star "ul"

let dd = star "dd"

let dt = star "dt"

let li = star "li"

let hr = terminal "hr"

let b = star "b"

let i = star "i"

let u = star "u"

let small = star "small"

let sub = star "sub"

let sup = star "sup"

let mark = star "mark"

let rp = star "rp"

let rt = star "rt"

let ruby = star "ruby"

let wbr = terminal "wbr"

(* VB *)
let bdo ~dir ?(a = []) elts = Xml.node ~a: ((a_dir dir) :: a) "bdo" elts

let a_datetime = string_attrib "datetime"

let a_shape = string_attrib "shape"

let a_coords coords = user_attrib string_of_numbers "coords" coords

let a_usemap = string_attrib "usemap"

let a_defer =
  constant_attrib "defer"

let a_label = string_attrib "label"

let area ~alt ?(a = []) () = Xml.leaf ~a: ((a_alt alt) :: a) "area"

let map = star "map"

let del = star "del"

let ins = star "ins"

let script = unary "script"

let noscript = star "noscript"

let article = star "article"

let aside = star "aside"

let main = star "main"

let audio = star "audio"

let video = star "video"

let canvas = star "canvas"

let command = terminal "command"

let menu = star "menu"

let embed = terminal "embed"

let source = terminal "source"

let meter = star "meter"

let output_elt = star "output"

let form = star "form"

let svg = star "svg"

let input = terminal "input"

let keygen = terminal "keygen"

let label = star "label"

let option = unary "option"

let select = star "select"

let textarea = unary "textarea"

let button = star "button"

let datalist = star "datalist"

let progress = star "progress"

let legend = star "legend"

let details = star "details"

let summary = star "summary"

let fieldset = star "fieldset"

let optgroup = star "optgroup"

let figcaption = star "figcaption"

let figure = star "figure"

let caption = star "caption"

let table = star "table"

let td = star "td"

let th = star "th"

let tr = star "tr"

let colgroup = star "colgroup"

let col = terminal "col"

let thead = star "thead"

let tbody = star "tbody"

let tfoot = star "tfoot"

let iframe = star "iframe"

let object_ = star "object"

let param = terminal "param"

let img ~src ?(a = []) () =
  let a = (a_src src) :: a in
  Xml.leaf ~a "img"

let meta = terminal "meta"

let style = unary "style"

let link ~href ?(a = []) () =
  Xml.leaf ~a: ((a_href href) :: a) "link"

