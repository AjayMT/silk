
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

let rec f2l f a1 a2 l = match l with
  | [] -> (a1, a2)
  | x :: xs ->
     let (a1, a2) = f a1 a2 x in
     f2l f a1 a2 xs

let map_join f l =
  let ff xs x =
    let+ x = f x in
    x :: xs
  in
  let+ l = flb ff [] l in
  List.rev l

let rec assoc key l = match l with
  | [] -> None
  | (x, y) :: xys ->
     if x = key then Some (y, xys)
     else assoc key xys
