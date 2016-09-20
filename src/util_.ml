let identity x = x

let (%) f g = fun x -> f (g x)

module Option = struct
  let map fn = function
    | Some x -> Some (fn x)
    | None -> None

  let bind fn = function
    | Some x -> fn x
    | None -> None

  let default d = function
    | Some x -> x
    | None -> d

  let may fn = function
    | Some x -> fn x
    | None -> ()

  let some x = Some x

  let get d = function
    | Some x -> x
    | None -> d

  let filter pred = function
    | Some x as rv when pred x -> rv
    | other -> None
end

module List = struct
  include List
  let rec any fn = function
    | [] -> false
    | x::xs -> if fn x then true else any fn xs
end
