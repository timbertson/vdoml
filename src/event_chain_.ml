open Util_
(* The path of an event is complex in passe. The main complexity is nesting a
 * html tree of event type 'child_msg inside one of 'msg. We _could_ traverse the
 * tree and mutate every eveny-related property when you embed a child inside another,
 * but that would be a lot of work (and not very efficient).
 *
 * Instead, we build up a type-unsafe context (chain). We attach transformer nodes
 * (functions of type 'child_msg -> 'msg) into the vdom tree. As
 * the VDOM tree is traversed, these conversion nodes are accumulated, resulting
 * in a chain of all conversion nodes from the current context up to the root node
 * being rendered.
 *
 * As long as all VDOM traversal maintains this invariant _and_ event emitting
 * happens only via this module's `emit` function, this module will apply the
 * appropriate (type-unsafe) coersions to maintain correctness.
 *
 * Since event observing requires essentially the same DOM-awareness as
 * event transformation, it's implemented via the same mechanism (but simpler,
 * as it's already type safe).
 *
 * The other complexity is that events may _also_ be emitted by calling
 * `Ui.emit instance message` from user code. This code _is_ aware of the
 * child type, not the parent type. So at the same time as we produce a
 * transformation node, we also produce a nested `emit` function which
 * (given the parent instance's emit function), transforms the event and then
 * passes it up.
 *)

module Event_chain = struct
  type 'msg observer = 'msg -> unit

  type ('child_msg, 'msg) child = {
    parent_of_child_message : 'child_msg -> 'msg;
  }

  type 'msg unsafe_child = {
    unsafe_parent_of_child_event : 'msg -> 'msg;
  }

  type 'msg node =
    | Child of 'msg unsafe_child
    | Observer of 'msg observer

  let observer observe = Observer observe

  module Identity : sig
    type 'a t
    val eq : 'a t -> 'a t -> bool
    val coerce_observer : ('a -> unit) -> 'a t
    val coerce_converter : ('a -> 'a) -> 'a t
  end = struct
    type 'a t = 'a -> 'a
    let coerce_observer obs = (Obj.magic obs)
    let coerce_converter conv = conv
    let eq a b = (a == b) (* functions can only ever be referntially equal *)
  end

  (* paths are built up by prefixing as we descend into a node
   * tree, so the first element is the innermost event node *)
  type 'msg path = {
    nodes: 'msg node list;
    identity: 'msg Identity.t list;
  }

  let init = {
    nodes = [];
    identity = [];
  }

  (* Coerce a child event node into a parent one.
   * This is only safe when access to 'msg event values for any
   * transformed nodes (or their descendants) goes via this module's
   * `emit` function *)
  let unsafe_coerce (child:('child_msg, 'msg) child) : 'msg node =
    let { parent_of_child_message } = child in
    Child {
      unsafe_parent_of_child_event = Obj.magic parent_of_child_message
    }

  let enter node { nodes; identity } =
    {
      nodes = node :: nodes;
      identity = (match node with
        | Child child -> Identity.coerce_converter child.unsafe_parent_of_child_event
        | Observer fn -> Identity.coerce_observer fn
      ) :: identity
    }

  let emit ~(toplevel:'msg -> unit) path (msg:'msg) = (
    let rec iter chain msg = match chain with
      | [] -> msg
      | head::tail ->
        let msg = match head with
          | Child child ->
            child.unsafe_parent_of_child_event msg
          | Observer obs ->
            obs msg;
            msg
        in
        iter tail msg
    in
    let msg : 'msg = iter path.nodes msg in
    toplevel msg
  )

  let eq a b =
    let rec eq a b =
      match a, b with
        | [], [] -> true
        | [], _ | _, [] -> false (* different lengths *)
        | a::atail, b::btail ->
          Identity.eq a b && eq atail btail
    in
    eq a.identity b.identity

  (* Note: for efficient Vdom diffing, `convert` should be statically-defined
   * rather than an anonymous function, as it's used in equality checking
   *)
  let child
    : ('child_msg -> 'msg)
    -> ('msg -> unit)
    -> ('child_msg -> unit) * ('child_msg, 'msg) child
  = fun convert emit ->
    let emit_fn = emit % convert in
    let node = { parent_of_child_message = convert } in
    (emit_fn, node)
end
