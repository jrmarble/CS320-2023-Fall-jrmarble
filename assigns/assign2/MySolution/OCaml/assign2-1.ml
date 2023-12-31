(*
//
Assign2-1: 10 points
//
Please implement mylist_length based
on pattern matching that computes the
length of a given mylist.
//
let rec
mylist_length(xs: 'a mylist): int = ...
//
*)
#use "./../../assign2.ml"
let rec
mylist_length(xs: 'a mylist): int =
  match xs with
  | MyNil -> 0 
  | MyCons(_, xs) -> 1 + mylist_length(xs)
  | MySnoc(xs, _) -> 1 + mylist_length(xs)
  | MyReverse(xs) -> mylist_length(xs)
  | MyAppend2(xs1, xs2) -> mylist_length(xs1) + mylist_length(xs2)
;;
