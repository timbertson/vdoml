type 'msg html = 'msg Vdom_.Vdom.html
type 'msg attr = 'msg Attr_.Attr.t
type 'msg event_handler

val emitter : ?response:Event.response -> 'msg-> 'msg event_handler
val handler: (Event.biggest_event Js.t -> 'msg Event.result) -> 'msg event_handler

(* General HTML builders *)
(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2011 Pierre Chambart, Grégoire Henry
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

(** A nullary element is an element that doesn't have any children. *)
type 'a nullary = ?a:('a attr list) -> unit -> 'a html

(** A unary element is an element that have exactly one children. *)
type 'a unary = ?a:('a attr list) -> 'a html -> 'a html

(** A star element is an element that has any number of children, including zero. *)
type 'a star =
  ?a:('a attr list) -> 'a html list -> 'a html

(** {2:attributes Attributes} *)

val a_class : string -> 'msg attr
(** This attribute assigns a class name or set of class names to an
    element. Any number of elements may be assigned the same class
    name or names.  *)

val a_user_data : string -> string -> 'msg attr
(** May be used to specify custom attributes.
    The example given by the W3C is as follows :
    {v
<ol>
<li data-length="2m11s">Beyond The Sea</li>
</ol> v}
    It should be used for preprocessing ends only. *)

val a_id : string -> 'msg attr
(** This attribute assigns a name to an element. This name must be
    unique in a document. The string should be without any space. *)

val a_title : string -> 'msg attr
(** This attribute offers advisory information about the element for
    which it is set.

    Values of the title attribute may be rendered by user agents in a
    variety of ways. For instance, visual browsers frequently display
    the title as a {i tool tip} (a short message that appears when the
    pointing device pauses over an object). Audio user agents may
    speak the title information in a similar context.

    The title attribute has an additional role when used with the [link]
    element to designate an external style sheet. Please consult the
    section on links and style sheets for details.  *)

(** {3 I18N} *)

val a_xml_lang : string -> 'msg attr

val a_lang : string -> 'msg attr

(** {3 Events}

    {4 Javascript events} *)

val a_onabort : 'msg event_handler -> 'msg attr
val a_onafterprint : 'msg event_handler -> 'msg attr
val a_onbeforeprint : 'msg event_handler -> 'msg attr
val a_onbeforeunload : 'msg event_handler -> 'msg attr
val a_onblur : 'msg event_handler -> 'msg attr
val a_oncanplay : 'msg event_handler -> 'msg attr
val a_oncanplaythrough : 'msg event_handler -> 'msg attr
val a_onchange : 'msg event_handler -> 'msg attr
val a_ondurationchange : 'msg event_handler -> 'msg attr
val a_onemptied : 'msg event_handler -> 'msg attr
val a_onended : 'msg event_handler -> 'msg attr
val a_onerror : 'msg event_handler -> 'msg attr
val a_onfocus : 'msg event_handler -> 'msg attr
val a_onformchange : 'msg event_handler -> 'msg attr
val a_onforminput : 'msg event_handler -> 'msg attr
val a_onhashchange : 'msg event_handler -> 'msg attr
val a_oninput : 'msg event_handler -> 'msg attr
val a_oninvalid : 'msg event_handler -> 'msg attr
val a_onmousewheel : 'msg event_handler -> 'msg attr
val a_onoffline : 'msg event_handler -> 'msg attr
val a_ononline : 'msg event_handler -> 'msg attr
val a_onpause : 'msg event_handler -> 'msg attr
val a_onplay : 'msg event_handler -> 'msg attr
val a_onplaying : 'msg event_handler -> 'msg attr
val a_onpagehide : 'msg event_handler -> 'msg attr
val a_onpageshow : 'msg event_handler -> 'msg attr
val a_onpopstate : 'msg event_handler -> 'msg attr
val a_onprogress : 'msg event_handler -> 'msg attr
val a_onratechange : 'msg event_handler -> 'msg attr
val a_onreadystatechange : 'msg event_handler -> 'msg attr
val a_onredo : 'msg event_handler -> 'msg attr
val a_onresize : 'msg event_handler -> 'msg attr
val a_onscroll : 'msg event_handler -> 'msg attr
val a_onseeked : 'msg event_handler -> 'msg attr
val a_onseeking : 'msg event_handler -> 'msg attr
val a_onselect : 'msg event_handler -> 'msg attr
val a_onshow : 'msg event_handler -> 'msg attr
val a_onstalled : 'msg event_handler -> 'msg attr
val a_onstorage : 'msg event_handler -> 'msg attr
val a_onsubmit : 'msg event_handler -> 'msg attr
val a_onsuspend : 'msg event_handler -> 'msg attr
val a_ontimeupdate : 'msg event_handler -> 'msg attr
val a_onundo : 'msg event_handler -> 'msg attr
val a_onunload : 'msg event_handler -> 'msg attr
val a_onvolumechange : 'msg event_handler -> 'msg attr
val a_onwaiting : 'msg event_handler -> 'msg attr
val a_onload : 'msg event_handler -> 'msg attr
val a_onloadeddata : 'msg event_handler -> 'msg attr
val a_onloadedmetadata : 'msg event_handler -> 'msg attr
val a_onloadstart : 'msg event_handler -> 'msg attr
val a_onmessage : 'msg event_handler -> 'msg attr

(** {4 Mouse events} *)

val a_onclick : 'msg event_handler -> 'msg attr
val a_oncontextmenu : 'msg event_handler -> 'msg attr
val a_ondblclick : 'msg event_handler -> 'msg attr
val a_ondrag : 'msg event_handler -> 'msg attr
val a_ondragend : 'msg event_handler -> 'msg attr
val a_ondragenter : 'msg event_handler -> 'msg attr
val a_ondragleave : 'msg event_handler -> 'msg attr
val a_ondragover : 'msg event_handler -> 'msg attr
val a_ondragstart : 'msg event_handler -> 'msg attr
val a_ondrop : 'msg event_handler -> 'msg attr
val a_onmousedown : 'msg event_handler -> 'msg attr
val a_onmouseup : 'msg event_handler -> 'msg attr
val a_onmouseover : 'msg event_handler -> 'msg attr
val a_onmousemove : 'msg event_handler -> 'msg attr
val a_onmouseout : 'msg event_handler -> 'msg attr

(** {4 Keyboard events} *)

val a_onkeypress : 'msg event_handler -> 'msg attr
val a_onkeydown : 'msg event_handler -> 'msg attr
val a_onkeyup : 'msg event_handler -> 'msg attr

(** {3 Other attributes} *)

val a_autocomplete : (bool[@onoff]) -> 'msg attr

val a_async : unit -> 'msg attr

val a_autofocus : unit -> 'msg attr

val a_autoplay : unit -> 'msg attr

val a_muted : unit -> 'msg attr

val a_crossorigin : string -> 'msg attr

val a_mediagroup : string -> 'msg attr

val a_challenge : string -> 'msg attr

val a_contenteditable : bool -> 'msg attr

val a_contextmenu : string -> 'msg attr

val a_controls : unit -> 'msg attr

val a_dir : string -> 'msg attr

val a_draggable : bool -> 'msg attr

val a_form : string -> 'msg attr

val a_formaction : string -> 'msg attr

val a_formenctype : string -> 'msg attr

val a_formnovalidate : unit -> 'msg attr

val a_formtarget : string -> 'msg attr

val a_hidden : unit -> 'msg attr

val a_high : float -> 'msg attr

val a_icon : string -> 'msg attr

val a_ismap : unit -> 'msg attr

val a_keytype : string -> 'msg attr

val a_list : string -> 'msg attr

val a_loop : unit -> 'msg attr

val a_low : float -> 'msg attr

val a_max : float -> 'msg attr

val a_input_max : int -> 'msg attr

val a_min : float -> 'msg attr

val a_input_min : int -> 'msg attr

val a_inputmode : string -> 'msg attr

val a_novalidate : unit -> 'msg attr

val a_open : unit -> 'msg attr

val a_optimum : float -> 'msg attr

val a_pattern : string -> 'msg attr

val a_placeholder : string -> 'msg attr

val a_poster : string -> 'msg attr

val a_preload : string -> 'msg attr

val a_pubdate : unit -> 'msg attr

val a_radiogroup : string -> 'msg attr

val a_required : unit -> 'msg attr

val a_reversed : unit -> 'msg attr

val a_sandbox : string -> 'msg attr

val a_spellcheck : bool -> 'msg attr

val a_scoped : unit -> 'msg attr

val a_seamless : unit -> 'msg attr

val a_sizes : string -> 'msg attr

val a_span : int -> 'msg attr

val a_srcset : string -> 'msg attr

val a_start : int -> 'msg attr

val a_step : string -> 'msg attr

val a_wrap : string -> 'msg attr

val a_version : string -> 'msg attr

val a_xmlns : string -> 'msg attr

val a_manifest : string -> 'msg attr

val a_cite : string -> 'msg attr

val a_xml_space : string -> 'msg attr

val a_accesskey : char -> 'msg attr
(** This attribute assigns an access key to an element. An access key
    is a single character from the document character
    set. NB: authors should consider the input method of the
    expected reader when specifying an accesskey. *)

val a_charset : string -> 'msg attr
(** This attribute specifies the character encoding of the resource
    designated by the link. Please consult the section on character
    encodings for more details. *)

val a_accept_charset : string list -> 'msg attr

val a_accept : string list -> 'msg attr

val a_href : string -> 'msg attr
(** This attribute specifies the location of a Web resource, thus
    defining a link between the current element (the source anchor)
    and the destination anchor defined by this attribute. *)

val a_hreflang : string -> 'msg attr
(** This attribute specifies the base language of the resource
    designated by href and may only be used when href is specified. *)

val a_download : string -> 'msg attr

val a_rel : string -> 'msg attr
(** This attribute describes the relationship from the current
    document to the anchor specified by the href attribute. The
    value of this attribute is a space-separated list of link
    types. *)

(** This attribute is used to describe a reverse link from the
    anchor specified by the href attribute to the current
    document. The value of this attribute is a space-separated
    list of link types. *)

val a_tabindex : int -> 'msg attr
(** This attribute specifies the position of the current
    element in the tabbing order for the current document. This
    value must be a number between 0 and 32767. User agents
    should ignore leading zeros. *)

val a_mime_type : string -> 'msg attr
(** This attribute gives an advisory hint as to the content type
    of the content available at the link target address. It
    allows user agents to opt to use a fallback mechanism rather
    than fetch the content if they are advised that they will
    get content in a content type they do not support.Authors
    who use this attribute take responsibility to manage the
    risk that it may become inconsistent with the content
    available at the link target address. *)

val a_datetime : string -> 'msg attr

val a_action : string -> 'msg attr
(** This attribute specifies a form processing agent. User agent
    behavior for a value other than an HTTP URI is undefined. *)

val a_checked : unit -> 'msg attr
(** When the [type] attribute has the value ["radio"] or
    ["checkbox"], this boolean attribute specifies that the
    button is on. User agents must ignore this attribute for
    other control types. *)

val a_cols : int -> 'msg attr
(** This attribute specifies the visible width in average
    character widths. Users should be able to enter longer lines
    than this, so user agents should provide some means to
    scroll through the contents of the control when the contents
    extend beyond the visible area. User agents may visible
    string lines to keep long lines visible without the need for
    scrolling. *)

val a_enctype : string -> 'msg attr

val a_for : string -> 'msg attr

val a_maxlength : int -> 'msg attr

val a_method :
  string -> 'msg attr

val a_multiple : unit -> 'msg attr

val a_name : string -> 'msg attr
(** This attribute assigns the control name. *)

val a_rows : int -> 'msg attr
(** This attribute specifies the number of visible string
    lines. Users should be able to enter more lines than this,
    so user agents should provide some means to scroll through
    the contents of the control when the contents extend beyond
    the visible area. *)

val a_selected : unit -> 'msg attr
(** When set, this boolean attribute specifies that
    this option is pre-selected. *)

val a_size : int -> 'msg attr

val a_src : string -> 'msg attr

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

val a_input_type : input_type -> 'msg attr

val a_text_value : string -> 'msg attr
(** This attribute specifies the initial value of the
    control. If this attribute is not set, the initial value is
    set to the contents of the [option] element. *)

val a_int_value : int -> 'msg attr

val a_value : string -> 'msg attr

val a_float_value : float -> 'msg attr

val a_disabled : unit -> 'msg attr

val a_readonly : unit -> 'msg attr

val a_button_type : input_type -> 'msg attr

val a_command_type :
  string -> 'msg attr

val a_menu_type : string -> 'msg attr

val a_label : string -> 'msg attr

val a_colspan : int -> 'msg attr

val a_headers : string list -> 'msg attr

val a_rowspan : int -> 'msg attr

val a_alt : string -> 'msg attr

val a_height : int -> 'msg attr

val a_width : int -> 'msg attr

val a_shape : string -> 'msg attr

val a_coords : int list -> 'msg attr

val a_usemap : string -> 'msg attr

val a_data : string -> 'msg attr

val a_scrolling : string -> 'msg attr

val a_target : string -> 'msg attr

val a_content : string -> 'msg attr

val a_http_equiv : string -> 'msg attr

val a_defer : unit -> 'msg attr

val a_media : string -> 'msg attr

val a_style : string -> 'msg attr

val a_property : string -> 'msg attr

(** {2:elements Elements} *)

val html :
  ?a: ('msg attr list) ->
  'msg html list -> 'msg html

val head :
  ?a: ('msg attr list) ->
  'msg html list -> 'msg html

val base : 'msg nullary

val title : 'msg unary

val body : 'msg star


val svg : ?a : 'msg attr list -> 'msg html list -> 'msg html

(** {3 Section} *)

val footer :
  'msg star

val header :
  'msg star

val section :
  'msg star

val nav : 'msg star

val h1 : 'msg star

val h2 : 'msg star

val h3 : 'msg star

val h4 : 'msg star

val h5 : 'msg star

val h6 : 'msg star

val hgroup :
  'msg star

val address :
  'msg star

val article :
  'msg star

val aside :
  'msg star

val main :
  'msg star

(** {3 Grouping content} *)

val p : 'msg star

val pre : 'msg star

val blockquote :
  'msg
    star

val div : 'msg star

val dl : 'msg star

val ol : 'msg star

val ul : 'msg star

val dd : 'msg star

val dt : 'msg star

val li : 'msg star

val figcaption :
  'msg star

val figure :
  'msg star

val hr : 'msg nullary

(** {3 Semantic} *)

val b : 'msg star

val i : 'msg star

val u : 'msg star

val small :
  'msg star

val sub : 'msg star

val sup : 'msg star

val mark : 'msg star

val wbr : 'msg nullary

val bdo : 'msg star

val abbr : 'msg star

val br : 'msg nullary

val cite : 'msg star

val code : 'msg star

val dfn : 'msg star

val em : 'msg star

val kbd : 'msg star

val q : 'msg star

val samp : 'msg star

val span : 'msg star

val strong :
  'msg star

val time : 'msg star

val var : 'msg star

(** {3 Hypertext} *)

val a : 'msg star

(** {3 Edit} *)

val del : 'msg star
val ins : 'msg star

(** {3 Embedded} *)

val img :
  src: string ->
  'msg nullary

val iframe :
  'msg star

val object_ :
  'msg star

val param : 'msg nullary

val embed :
  'msg nullary

val audio :
  'msg star

val video :
  'msg star

val canvas : 'msg star

val source : 'msg nullary

val area :
  alt: string ->
  'msg nullary

val map : 'msg star

(** {3 Tables Data} *)

val caption :
  'msg star

val table :
  'msg star

val colgroup :
  'msg star

val col : 'msg nullary

val thead :
  'msg star

val tbody :
  'msg star

val tfoot :
  'msg star

val td : 'msg star

val th : 'msg star

val tr : 'msg star

(** {3 Forms} *)

val form : 'msg star

val fieldset :
  'msg star

val legend :
  'msg star

val label :
  'msg star

val input : 'msg nullary

val button :
  'msg star

val select :
  'msg star

val datalist :
    'msg star

val optgroup :
  'msg star

val option :
  'msg unary

val textarea :
  'msg unary

val keygen : 'msg nullary

val progress :
  'msg star

val meter :
  'msg star

val output_elt :
  'msg star

(** {3 Data} *)

val text : string -> 'msg html

val entity : string -> 'msg html

val space : unit -> 'msg html


(** {3 Interactive} *)

val details :
  'msg star

val summary :
  'msg star

val menu :
    'msg star

(** {3 Scripting} *)

val script :
  'msg unary

val noscript :
  'msg star

val meta : 'msg nullary

(** {3 Style Sheets} *)

val style :
  'msg unary

val link : href:string -> 'msg nullary

(** {3 Ruby} *)

val rt : 'msg star

val rp : 'msg star

val ruby : 'msg star

val onoff_of_bool : bool -> string

val string_of_input_type : input_type -> string

val string_of_numbers : int list -> string
