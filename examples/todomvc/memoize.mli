type ('arguments, 'ret) cached_function = 'arguments -> 'ret
val init : ('arguments -> 'ret) -> ('arguments, 'ret) cached_function
