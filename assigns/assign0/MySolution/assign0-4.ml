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
Assign0-4: 10 points
Please implement a function that converts a given
string to an integer:
fun str2int(cs: string): int
In particular, it is expected that str2int(int2str(x)) = x
*)

let rec
makeInt(cs: string)(i: int)(n: int): int =
if string_length(cs) = i+1 then n*10 + (ord(string_get(cs,i))-48) 
else makeInt(cs)(i+1)((n*10 + ord(string_get(cs,i))-48))
;;

let rec
str2int(cs: string): int =
if ord(string_get(cs,0)) = ord('-') then -1 * makeInt(cs)(1)(0) 
else makeInt(cs)(0)(0)
;;

(* ****** ****** *)
