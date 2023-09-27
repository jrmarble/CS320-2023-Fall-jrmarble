(*
//
Assign2-2: 10 points
//
Please implement mylist_get_at based
on pattern matching that computes the
element of a given mylist at a given
position.
//
You should call mylist_subscript_exn if
the give position is out-of-bounds.
//
let rec
mylist_get_at(xs: 'a mylist)(i0: int): 'a = ...
//
*)
#use "./../../assign2.ml"

let rec 
mylist_get_at(xs: 'a mylist)(i0: int): 'a =
  if (i0 < 0) then mylist_subscript_exn()
  else
    match xs with
    | MyNil -> mylist_subscript_exn()
    | MyCons(x, rest) ->
        if (i0=0) then x
        else mylist_get_at(rest)(i0-1)
    | MySnoc(rest, x) ->
        if i0 = (mylist_length(rest)) then x
        else mylist_get_at(rest)(i0)
    | MyReverse(rest) ->
        mylist_get_at(rest)((mylistlength(rest))-1-i0)
    | MyAppend2(xs1, xs2) ->
        let length1 = mylist_length(xs1) in
        if (i0<length1) then mylist_get_at(xs1)(i0)
        else mylist_get_at(xs2)(i0-length1)
  
and mylist_length(xs: 'a mylist): int = 
  match xs with
  | MyNil -> 0
  | MyCons(_, xs) -> 1 + mylist_length(xs)
  | MySnoc(xs, _) -> 1 + mylist_length(xs)
  | MyReverse(xs) -> mylist_length(xs)
  | MyAppend2(xs1, xs2) -> mylist_length(xs1) + mylist_length(xs2)
;;
