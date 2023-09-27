(*
//
Assign2-3: 10 points
//
Please implement foldleft_to_iforeach that turns a
foldleft-loop into a iforeach-loop:
let
foldleft_to_iforeach
(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = ...
*)

#use "./../../../classlib/OCaml/MyOCaml.ml"
#use "./../../assign2.ml"

let 
foldleft_to_iforeach(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = 
  fun xs action ->
    let foldHelp(idx)(x) =
      action(idx)(x);
      idx + 1
    in
    let _ = foldleft(xs)(0)(helper)
    in
    ()
;;