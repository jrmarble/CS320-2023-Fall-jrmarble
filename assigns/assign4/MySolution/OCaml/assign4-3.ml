(*
//
Assign4-3:
//
HX-2023-10-05: 10 points
//
Please enumerate a gtree in the manner of
depth-first search:
//
let rec (* 5 points *)
gtree_streamize_dfs(xs: 'a gtree): 'a stream
//
Please enumerate a gtree in the manner of
breadth-first search:
//
let rec (* 5 points *)
gtree_streamize_bfs(xs: 'a gtree): 'a stream
//
*)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a gtree =
| GTnil | GTcons of 'a * ('a gtree list)

let fchildren (x0: 'a gtree): 'a gtree list =
  match x0 with
  | GTnil -> []
  | GTcons(_, xs) -> xs 
;;

let rec
gtree_dfs
( nxs
: 'node list)
( fchildren
: 'node -> 'node list): 'node stream = fun() ->
(
match nxs with
  [] -> StrNil
| nx1 :: nxs ->
  StrCons(nx1, gtree_dfs(fchildren(nx1) @ nxs)(fchildren))
)
;;

let rec
gtree_bfs
( nxs
: 'node list)
( fchildren
: 'node -> 'node list): 'node stream = fun() ->
(
match nxs with
  [] -> StrNil
| nx1 :: nxs ->
  StrCons(nx1, gtree_bfs(nxs @ fchildren(nx1))(fchildren))
)
;;


let rec extractStreams(resStream) = fun() ->
  match resStream() with
  | StrNil -> StrNil
  | StrCons(x,xs) ->
    match x with 
    | GTnil -> extractStreams xs ()
    | GTcons(x1, rest) -> StrCons(x1, extractStreams xs)
;;

let rec gtree_streamize_dfs(xs: 'a gtree): 'a stream =
  fun() -> extractStreams(gtree_dfs([xs])(fchildren))()
;;

let rec gtree_streamize_bfs(xs: 'a gtree): 'a stream =
  fun() -> extractStreams(gtree_bfs([xs])(fchildren))()
;;