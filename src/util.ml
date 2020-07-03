
(*
 * Utilities.
 *)

let ( let* ) x f = Result.bind x f
let ( let+ ) x f = Result.map f x

let rec flb f acc l = match l with
  | [] -> Ok acc
  | (x :: xs) ->
     let* a = f acc x in
     flb f a xs

let map_join f l =
  let ff xs x =
    let+ x = f x in
    x :: xs
  in
  let+ l = flb ff [] l in
  List.rev l

let assoc2 key l =
  let f found pair =
    let (k, v) = pair in
    let v = if k = key then Some v else None in
    match found with
    | (Some _, Some _) -> found
    | (Some a, None) -> (Some a, v)
    | (None, _) -> (v, None)
  in
  List.fold_left f (None, None) l
