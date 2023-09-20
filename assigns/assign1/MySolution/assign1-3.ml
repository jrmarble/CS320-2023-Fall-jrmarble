(*
assign1-3: 10 points
A 3-letter sequence abc is 132-like
if a < c < b holds. For instance, 123 is
not 132-like; but 596 is 132-like.

A string is 132-avoid if there is no subsequence
abc in this string that is 132-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 132-avoid;
For instance, 987654321 is 132-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 165 is 132-like.

Please implement a function string_avoid_132 that
checks if a given string is 132-avoid; the function
returns true if and only if the given string is 132-
avoid.

fun string_avoid_132(cs: string): bool
*)

#use "./../../../classlib/OCaml/MyOcaml.ml";;

let string_avoid_132(cs: string): bool=
  let n = string (length)(cs) in
  let rec loop(i)(j)(k) = 
    if i >= n-3 then true
    else if j >= n-2 then loop(i + 1)(i + 2)(i + 3)
    else if k >= n-1 then loop(i)(g + 1)(j + 2)
    else if cs.[i] < cs.[k] && cs.[k] < cs.[j] then false 
    else loop(i)(j)(k + 1)
  in
  loop(0)(1)(2)(3)
;;
