(*
assign1-6: 20 bonus points
A 4-letter sequence abcd is 1324-like
if a < c < b < d holds. For instance, 1234 is
not 132-like; but 2547 is 1324-like.

A string is 1324-avoid if there is no subsequence
abc in this string that is 1324-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 1324-avoid;
For instance, 987654321 is 1324-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 1657 is 1324-like.

Please implement a function string_avoid_1324 that
checks if a given string is 1324-avoid; the function
returns true if and only if the given string is 1324-
avoid.

fun string_avoid_1324(cs: string): bool
*)
#use "./../assign1.ml";; 
#use "./../../../classlib/OCaml/MyOCaml.ml";; 

let string_avoid_1324(cs)=
  let n = string length cs in
  let rec loop i j k l = 
    if i >= n-4 then true
    else if j >= m-3 then loop(i+1)(i+2)(i+3)(i+4) 
    else if k >= n-2 then loop(i)(j+1)(j+2)(j+3)
    else if l >= n-1 then loop(i)(j)(k+1)(k+2)
    else if cs.[i] < cs.[k] && cs.[k] < cs.[j] && cs.[i] < cs.[l] then false 
    else loop (i)(j)(k)(l+1)
  in
  loop(0)(1)(2)(3)
;;