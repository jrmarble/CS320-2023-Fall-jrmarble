(*
assign1-5: 20 points

A sequence of chars is ascending if any char in
the sequence is less than or equal to the following
one (when the following one does exist).
Given a string cs, please implement a function
that find the longest ascending subsequence of [cs].
If there are more than one such sequences, the left
most one should be returned.

fun string_longest_ascend(xs: string): string

For instance, given "1324561111", the function
string_longest_ascend returns "13456"

For instance, given "1234561111", the function
string_longest_ascend returns "123456"

For instance, given "1234511111", the function
string_longest_ascend returns "111111".
*)
#use "./../../../classlib/OCaml/MyOcaml.ml";;

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