type ('arguments, 'ret) cached_function
val init : ('arguments -> 'ret) -> ('arguments, 'ret) cached_function
val apply : ('arguments, 'ret) cached_function -> 'arguments -> 'ret
