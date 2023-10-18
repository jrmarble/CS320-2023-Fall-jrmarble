(*
//
Assign4-1:
//
HX-2023-10-05: 10 points
//
The following is a well-known series:
ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...
Please implement a stream consisting of all the
partial sums of this series.
The 1st item in the stream equals 1
The 2nd item in the stream equals 1 - 1/2
The 3rd item in the stream equals 1 - 1/2 + 1/3
The 4th item in the stream equals 1 - 1/2 + 1/3 - 1/4
And so on, and so forth
//
let the_ln2_stream: float stream = fun() -> ...
//
*)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type float_stream = unit -> float_stream_node
and float_stream_node =
  | StrNil
  | StrCons of float * float_stream

let the_ln2_stream =
  let rec aux(n)(sum)(sign) =
    let x = sign *. (1.0 /. n) in
    fun () -> StrCons(sum +. x,aux(n +. 1.0)(sum +. x)(-. sign))
  in
  aux(1.0)(0.0)(1.0)
