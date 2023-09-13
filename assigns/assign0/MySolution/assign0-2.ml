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
Assign0-2: 10 points
Please implement a function that tests whether a
given natural number is a prime:
fun isPrime(n0: int): bool
*)
let rec
primeHelper(n0: int)(n1: int): bool = 
if n1 = 1 then true 
else if n0 mod n1 = 0 then false 
else primeHelper(n0)(n1-1)
;;


let rec
isPrime(n0: int): bool = 
if n0 = 0 then false else if n0 = 1 then true 
else primeHelper(n0)(n0-1)
;;
(* ****** ****** *)
