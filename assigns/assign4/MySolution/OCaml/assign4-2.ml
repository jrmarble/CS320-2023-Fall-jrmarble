(*
//
Assign4-2:
//
HX-2023-10-05: 10 points
//
Please enumerate all the pairs of natural
numbers. Given pairs (i1, j1) and (i2, j2),
(i1, j1) should be enumerated ahead of (i2, j2)
if i1+j1 < i2+j2.
//
let theNatPairs: (int*int) stream = fun () -> ...
//
*)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a strcon = StrNil | StrCons of 'a * (unit -> 'a strcon);;

let rec natnums_from n () = StrCons(n, natnums_from (n+1));;

let natNums = natnums_from 0;;

let cross_product(xs)(ys) =
    let rec cp(xrest)(yrest)(sum)(i)() =
        if i > sum then
            next_cp(xrest)(yrest)(sum+1)(0)()
        else
            StrCons((i, sum - i), cp(xrest)(yrest)(sum)(i+1))
    and next_cp(xrest)(yrest)(sum)(i)() =
        let (x, xtail) = match xrest () with 
        StrCons(v, tail) -> (v, tail) 
        | StrNil -> (0, fun () -> StrNil) in
        cp (xtail)(yrest)(sum)(i)()
    in
    cp(xs)(ys)(0)(0)
;;

let theNatPairs = cross_product(natNums)(natNums);;
