(* The path of an event is complex in vdoml. A traversal of VDom nodes
 * requires tracking not just content, but the accumulation
 * of Conversion and Observer psuedo-nodes.
 *
 * In particular, when diffing it's important to only diff "corresponding"
 * nodes, which we restrict to being "the exact same chain of conversion
 * and observer functions". Since functions can only be compared by
 * reference, we can't compose these functions directly but rather store
 * them in a nested object for later comparison.
 *
 * This event chain uses an existential type to hide the "midpoints" of
 * the chain, which gives us compatible types when building nodes. However
 * this means that when traversing nodes we need to use Obj.magic casts
 * in order to traverse two nodes of type 'a t and 'b t when we know
 * that a == b.
 *)

module Event_chain : sig
  type 'a t

  val eq : 'a t -> 'a t -> bool
  val mapper_eq : ('a -> 'b) -> ('c -> 'd) -> bool
  val emit : 'a t -> 'a -> unit

  val emitter : ('a -> unit) -> 'a t
  val add_observer : ('a -> unit) -> 'a t -> 'a t
  val add_conversion : ('a -> 'b) -> 'b t -> 'a t
end = struct
  type ('a, 'b) mapper = 'a -> 'b
  type 'a emitter = 'a -> unit
  type ('a) chain =
    | Chain_root: 'a emitter -> 'a chain
    | Chain_observer: ('a emitter * 'a chain) -> 'a chain
    | Chain_conversion: (('a, 'intermediate) mapper * 'intermediate chain) -> 'a chain

  type 'a t = 'a chain

  (* mapper_eq and coerce_chain can be applied with arbitrary type paramaters.
   * Equality checking is restricted to referencial equality, so
   * we can discount the generic type. If mappers are equal, their type
   * paramaters must have been equal.
   *)
  let mapper_eq : ('a, 'b) mapper -> ('c, 'd) mapper -> bool = fun a b ->
    let a : ('c, 'd) mapper = Obj.magic a in
    a == b

  let coerce_chain : ('a, 'b) mapper -> ('a, 'c) mapper -> 'b chain -> ('c chain) option = fun a b chain ->
    if mapper_eq a b then Some (Obj.magic chain) else None

  let emitter_eq : 'a emitter -> 'a emitter -> bool = fun a b -> a = b

  let rec eq : type a. a chain -> a chain -> bool = fun a b ->
    match (a, b) with
      | Chain_root a, Chain_root b -> emitter_eq a b
      | Chain_observer (a, a_chain), Chain_observer (b, b_chain) -> emitter_eq a b && eq a_chain b_chain
      | Chain_conversion (a, a_chain), Chain_conversion (b, b_chain) -> (
        match coerce_chain a b a_chain with
          | Some a_chain -> eq a_chain b_chain
          | None -> false
      )
      | Chain_root _, _ | Chain_observer _, _ | Chain_conversion _, _ -> false (* mismatched length / types *)

  let rec emit : type a. a chain -> a -> unit = fun chain msg ->
    match chain with
      | Chain_root emit -> emit msg
      | Chain_observer (observer, chain) -> emit chain msg; observer msg
      | Chain_conversion (map, chain) -> emit chain (map msg)

  (* builders *)
  let emitter emit = Chain_root emit
  let add_observer fn chain = Chain_observer (fn, chain)
  let add_conversion fn chain = Chain_conversion (fn, chain)
end
