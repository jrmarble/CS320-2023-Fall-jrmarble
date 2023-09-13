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
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)

let rec 
intLength(i0: int): int =
if i0/10 = 0 then 1 else 1 + intLength(i0/10)
;;

let rec 
getNum(i0: int)(i: int): int =
if intLength(i0) = i+1 then i0 mod 10 else getNum(i0/10)(i)
;;

let rec
int2strNeg(i0: int): string =
string_init(intLength(i0)+1)(fun i -> if i=0 then '-' 
else chr(getNum(i0)(i-1) + ord '0'))
;;
let rec
int2str(i0: int): string =
if i0 < 0 then int2strNeg(i0 * -1) else
string_init(intLength(i0))(fun i -> chr(getNum(i0)(i) + ord '0'))
;;

(* ****** ****** *)
