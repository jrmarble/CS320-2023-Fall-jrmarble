(* ****** ****** *)
let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0
;;
(* ****** ****** *)
let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0
;;
(* ****** ****** *)


(*
Assign0-1: 10 points
Please find the first integer N such that the
evaluation of fact(N) in OCaml yields an Overflow
exception.
*)
let rec
fact(x: int): int =
if x > 0 then x *fact(x-1) else 1
;;

let fact10 = fact(10)
;;
let fact100 = fact(100)
;;

let rec
myloop(x: int): int =
if fact(x) = 0 then x else myloop(x+1)
;;

let myans = myloop(0);;



(* ****** ****** *)
