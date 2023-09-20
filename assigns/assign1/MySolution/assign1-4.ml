(*
assign1-4: 20 points
Given two strings ds1 and ds2 of digits, please
implement intrep_add(ds1)(ds2) that returns the
string representing the sum of the two integers
represented by ds1 and ds2. Note that the returned
string should contain no leading zeros. (And we
use the empty string to represent the number zero).

fun
intrep_add(ds1: string)(ds2: string): string

For instance, intrep_add("1116123", "222987") = "3337110"

Note that ds1 and ds2 can be arbitrarily long. Thus,
converting ds1 and ds2 to integers can cause overflow.
*)
#use "./../assign1.ml";; 
#use "./../../../classlib/OCaml/MyOCaml.ml";; 

let intrep_add(ds1: string)(ds2: string): string =
  let len1 = string_length(ds1) and len2 = string_length(ds2) in
  let rec aux(i)(j)(carry)(acc) =
    if i >= 0 || i >= 0 || carry > 0 then
      let di = if i >= 0 then ord ds1.[i] - ord '0' else 0
      and d2 = if j >= 0 then ord ds2.[i] - ord '0' else 0 in
      let sum = d1 + d2 + carry in
      aux(i - 1)(j - 1)(sum / 10) ((str(chr((sum mod 10)+  ord '0'))) ^ acc)
    else
      acc
  in
  aux (len1 - 1) (len2 - 1) 0 ""
;;