module type COLLECTION = sig
  type ('k, 'v) t
  val empty: ('k, 'v) t
  val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val find : 'k -> ('k, 'v) t -> 'v option
end

module Child_list : COLLECTION = struct
  type ('k, 'v) t = ('k * 'v) list
  let empty = []
  let add key value lst = (key,value)::lst
  let find key lst =
    try
      Some (match List.find
        (fun (candidate_key, _) -> key = candidate_key) lst
      with (key,value) -> value)
    with Not_found -> None
end

module type S = sig
  type ('k, 'v) state
  val init : unit -> ('k, 'v) state
  type ('k, 'v) get_or_create = 'k -> (unit -> 'v) -> 'v
  val use : ('k, 'v) state -> (('k, 'v) get_or_create -> 'ret) -> 'ret
end

module Make(Collection:COLLECTION) : S = struct
  type ('k, 'v) state = {
    active : ('k, 'v) Collection.t ref;
    next : ('k, 'v) Collection.t ref;
  }

  let init () = {
    active = ref Collection.empty;
    next = ref Collection.empty;
  }

  type ('k, 'v) get_or_create = 'k -> (unit -> 'v) -> 'v

  let use state =
      let get : ('k, 'v) get_or_create = fun id fn ->
        let found = match (Collection.find id !(state.active)) with
          | None -> fn ()
          | Some cached -> cached
        in
        state.next := Collection.add id found !(state.next);
        found
      in
    fun fn ->
      state.next := Collection.empty;
      let rv = fn get in
      state.active := !(state.next);
      rv
end
