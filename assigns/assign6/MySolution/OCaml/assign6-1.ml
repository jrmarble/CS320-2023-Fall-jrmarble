(* ****** ****** *)
(*
//
Assign6:
Parsing and parsing combinators
//
DUE: the 13th of November, 2023
//
Except for the basic arithmetic functions
(including those on chars), you may only use
the functions in classlib/OCaml/MyOCaml.ml
//
*)
(* ****** ****** *)

(*
//
Assign6-1:
//
Please implement a print and parse function. Using parser combinators. When
given a valid string according to the grammar, your parse function returns an
sexpr value encoding the expression.

//
let sexpr_to_string (e : sexpr)  : string       = ...
let sexpr_parse     (s : string) : sexpr option = ...
//

Example (Accepted Strings):
parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])
//
Example (Rejected Strings):
parse "()" = None
parse "(add)" = None
parse "(add 1 2))" = None
parse "((mul 1 2)" = None
//
*)

(* ****** ****** *)

(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<exprs> ::= <expr> | <expr> <exprs>
<expr>  ::= <num>
          | (add <exprs> )
          | (mul <exprs> )
*)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)

  (* turn a string into a list of chars *)
  let string_listize (s : string) : char list =
    list_make_fwork(fun work -> string_foreach s work)
  
  let string_of_list (cs : char list) : string =
    string_make_fwork(fun work -> list_foreach cs work)
  
  (* remove blank chars at the front of a list *)
  let rec trim cs =
    match cs with
    | [] -> cs
    | '\n' :: cs -> trim cs
    | '\t' :: cs -> trim cs
    | '\r' :: cs -> trim cs
    | ' ' :: cs -> trim cs
    | _ -> cs

let rec sexpr_to_string (e: sexpr): string =
  match e with
  | SInt x -> string_of_int x
  | SAdd exprs -> "(" ^ "add " ^ (sexprs_to_string exprs) ^ ")"
  | SMul exprs -> "(" ^ "mul " ^ (sexprs_to_string exprs) ^ ")"

and sexprs_to_string exprs =
  match exprs with
  | [] -> ""
  | [e] -> sexpr_to_string e
  | e :: es -> sexpr_to_string e ^ " " ^ sexprs_to_string es


let rec parse_sexpr () : sexpr parser =
  parse_int () <|> parse_add () <|> parse_mul ()
  
and parse_int () : sexpr parser =
  let* n = natural in
  pure (SInt n) << whitespaces
  
and parse_add () : sexpr parser =
  let* _ = keyword "(add" in
  let* es = many1' parse_sexpr in
  let* _ = keyword ")" in
  pure (SAdd es)
  
and parse_mul () : sexpr parser =
  let* _ = keyword "(mul" in
  let* es = many1' parse_sexpr in
  let* _ = keyword ")" in
  pure (SMul es)
  
let sexpr_parse (s : string) : sexpr option =
  match string_parse (parse_sexpr ()) s with
  | Some (e, []) -> Some e
  | _ -> None


(* end of [CS320-2023-Fall-assigns-assign6.ml] *)