module type COLLECTION = sig
  type ('k, 'v) t
  val empty: ('k, 'v) t
  val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val find : 'k -> ('k, 'v) t -> 'v option
end

module Child_list : COLLECTION

module type S = sig
  type ('k, 'v) state
  val init : unit -> ('k, 'v) state
  type ('k, 'v) get_or_create = 'k -> (unit -> 'v) -> 'v
  val use : ('k, 'v) state -> (('k, 'v) get_or_create -> 'ret) -> 'ret
end

module Make(Collection:COLLECTION) : S
