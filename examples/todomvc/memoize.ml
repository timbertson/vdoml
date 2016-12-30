type ('arguments, 'ret) cached_function = 'arguments -> 'ret
type ('arguments, 'ret) cache = {
  fn: ('arguments -> 'ret);
  current: ('arguments * 'ret) option ref;
}
let init fn =
  let cache = { fn; current = ref None } in
  fun args ->
    match !(cache.current) with
      | Some (old_args, old_ret) when old_args = args ->
        old_ret
      | Some _ | None ->
        let ret = cache.fn args in
        cache.current := Some (args, ret);
        ret
