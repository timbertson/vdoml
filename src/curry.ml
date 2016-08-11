module Curry : sig
  type ('arguments, 'ret) cached_function
  val init : ('arguments -> 'ret) -> ('arguments, 'ret) cached_function
  val apply : ('arguments, 'ret) cached_function -> 'arguments -> 'ret
end = struct
  type ('arguments, 'ret) cached_function = {
    fn: ('arguments -> 'ret);
    current: ('arguments * 'ret) option ref;
  }
  let init fn = { fn; current = ref None }
  let apply cache args = 
    match !(cache.current) with
      | Some (old_args, old_ret) when old_args = args -> old_ret
      | Some _ | None ->
        let ret = cache.fn args in
        cache.current := Some (args, ret);
        ret
end

include Curry
