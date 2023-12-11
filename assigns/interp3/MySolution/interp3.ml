#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the [compile] function following the
specifications described in CS320_Fall_2023_Project-3.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(* ------------------------------------------------------------ *)

(* abstract syntax tree of high-level language *)

type uopr =
  | Neg | Not

type bopr =
  | Add | Sub | Mul | Div | Mod
  | And | Or
  | Lt  | Gt  | Lte | Gte | Eq

type expr =
  | Int of int
  | Bool of bool
  | Unit
  | UOpr of uopr * expr
  | BOpr of bopr * expr * expr
  | Var of string
  | Fun of string * string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Seq of expr * expr
  | Ifte of expr * expr * expr
  | Trace of expr

(* ------------------------------------------------------------ *)

(* combinator for left-associative operators *)

let chain_left (p : 'a parser) (q : ('a -> 'a -> 'a) parser) : 'a parser =
  let* init = p in
  let* fms = many (let* f = q in let* m = p in pure (f, m)) in
  let m = list_foldleft fms init (fun acc (f, m) -> f acc m) in
  pure m

let rec chain_right (p : 'a parser) (q : ('a -> 'a -> 'a) parser) : 'a parser =
  let* m = p in
  (let* f = q in
   let* rest = chain_right p q in
   pure (f m rest)) <|> 
  (pure m)

let opt (p : 'a parser) : 'a option parser =
  (let* x = p in pure (Some x)) <|> pure None

(* basic constants *)

let parse_int : expr parser =
  let* n = natural in
  pure (Int n) << whitespaces

let parse_bool : expr parser =
  (keyword "true" >> pure (Bool true)) <|>
  (keyword "false" >> pure (Bool false))

let parse_unit : expr parser =
  keyword "(" >> keyword ")" >> pure Unit

(* names *)

let isReserved s =
  let reserved = 
    ["let"; "rec"; "in"; "fun"; "if"; "then"; "else"; "trace"; "mod"; "not"] 
  in
  list_exists reserved (fun s0 -> s0 = s)

let parse_name : string parser =
  let lower = satisfy char_islower in
  let upper = satisfy char_isupper in
  let digit = satisfy char_isdigit in
  let quote = char '\'' in
  let wildc = char '_' in
  let* c = lower <|> wildc in
  let* cs = many (lower <|> upper <|> digit <|> wildc <|> quote) in
  let s = string_make_fwork (list_foreach (c :: cs)) in
  if isReserved s then fail
  else pure s << whitespaces

(* unary operators *)

let parse_neg : (expr -> expr) parser =
  keyword "-" >> pure (fun m -> UOpr (Neg, m))

(* binary operators *)

let parse_add : (expr -> expr -> expr) parser =
  keyword "+" >> pure (fun m n -> BOpr (Add, m, n))

let parse_sub : (expr -> expr -> expr) parser =
  keyword "-" >> pure (fun m n -> BOpr (Sub, m, n))

let parse_mul : (expr -> expr -> expr) parser =
  keyword "*" >> pure (fun m n -> BOpr (Mul, m, n))

let parse_div : (expr -> expr -> expr) parser =
  keyword "/" >> pure (fun m n -> BOpr (Div, m, n))

let parse_mod : (expr -> expr -> expr) parser =
  keyword "mod" >> pure (fun m n -> BOpr (Mod, m, n))

let parse_and : (expr -> expr -> expr) parser =
  keyword "&&" >> pure (fun m n -> BOpr (And, m, n))

let parse_or : (expr -> expr -> expr) parser =
  keyword "||" >> pure (fun m n -> BOpr (Or, m, n))

let parse_lt : (expr -> expr -> expr) parser =
  keyword "<" >> pure (fun m n -> BOpr (Lt, m, n))

let parse_gt : (expr -> expr -> expr) parser =
  keyword ">" >> pure (fun m n -> BOpr (Gt, m, n))

let parse_lte : (expr -> expr -> expr) parser =
  keyword "<=" >> pure (fun m n -> BOpr (Lte, m, n))

let parse_gte : (expr -> expr -> expr) parser =
  keyword ">=" >> pure (fun m n -> BOpr (Gte, m, n))

let parse_eq : (expr -> expr -> expr) parser =
  keyword "=" >> pure (fun m n -> BOpr (Eq, m, n))

let parse_neq : (expr -> expr -> expr) parser =
  keyword "<>" >> pure (fun m n -> UOpr (Not, BOpr (Eq, m, n)))

let parse_seq : (expr -> expr -> expr) parser =
  keyword ";" >> pure (fun m n -> Seq (m, n))

(* expression parsing *)

let rec parse_expr () = 
  let* _ = pure () in
  parse_expr9 ()

and parse_expr1 () : expr parser = 
  let* _ = pure () in
  parse_int <|> 
  parse_bool <|> 
  parse_unit <|>
  parse_var () <|>
  parse_fun () <|>
  parse_letrec () <|>
  parse_let () <|>
  parse_ifte () <|>
  parse_trace () <|>
  parse_not () <|>
  (keyword "(" >> parse_expr () << keyword ")")

and parse_expr2 () : expr parser =
  let* m = parse_expr1 () in
  let* ms = many' parse_expr1 in
  let m = list_foldleft ms m (fun acc m -> App (acc, m)) in
  pure m

and parse_expr3 () : expr parser =
  let* f_opt = opt parse_neg in
  let* m = parse_expr2 () in
  match f_opt with
  | Some f -> pure (f m)
  | None -> pure m

and parse_expr4 () : expr parser =
  let opr = parse_mul <|> parse_div <|> parse_mod in
  chain_left (parse_expr3 ()) opr

and parse_expr5 () : expr parser =
  let opr = parse_add <|> parse_sub in
  chain_left (parse_expr4 ()) opr

and parse_expr6 () : expr parser =
  let opr = 
    parse_lte <|> 
    parse_gte <|>
    parse_neq <|>
    parse_lt <|> 
    parse_gt <|>
    parse_eq
  in
  chain_left (parse_expr5 ()) opr

and parse_expr7 () : expr parser =
  chain_left (parse_expr6 ()) parse_and

and parse_expr8 () : expr parser =
  chain_left (parse_expr7 ()) parse_or

and parse_expr9 () : expr parser =
  chain_right (parse_expr8 ()) parse_seq

and parse_var () : expr parser =
  let* x = parse_name in
  pure (Var x)

and parse_fun () : expr parser =
  let* _ = keyword "fun" in
  let* xs = many1 parse_name in 
  let* _ = keyword "->" in
  let* body = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure m

and parse_let () : expr parser =
  let* _ = keyword "let" in
  let* x = parse_name in
  let* xs = many parse_name in
  let* _ = keyword "=" in
  let* body = parse_expr () in
  let* _ = keyword "in" in
  let* n = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure (Let (x, m, n))

and parse_letrec () : expr parser =
  let* _ = keyword "let" in
  let* _ = keyword "rec" in
  let* f = parse_name in
  let* x = parse_name in
  let* xs = many parse_name in
  let* _ = keyword "=" in
  let* body = parse_expr () in
  let* _ = keyword "in" in
  let* n = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure (Let (f, Fun (f, x, m), n))

and parse_ifte () : expr parser =
  let* _ = keyword "if" in
  let* m = parse_expr () in
  let* _ = keyword "then" in
  let* n1 = parse_expr () in
  let* _ = keyword "else" in
  let* n2 = parse_expr () in
  pure (Ifte (m, n1, n2))

and parse_trace () : expr parser =
  let* _ = keyword "trace" in
  let* m = parse_expr1 () in
  pure (Trace m) 

and parse_not () : expr parser =
  let* _ = keyword "not" in
  let* m = parse_expr1 () in
  pure (UOpr (Not, m))

exception SyntaxError
exception UnboundVariable of string

type scope = (string * string) list

let new_var =
  let stamp = ref 0 in
  fun x ->
    incr stamp;
    let xvar = string_filter x (fun c -> c <> '_' && c <> '\'') in
    string_concat_list ["v"; xvar; "i"; string_of_int !stamp]

let find_var scope s =
  let rec loop scope =
    match scope with
    | [] -> None
    | (s0, x) :: scope ->
      if s = s0 then Some x
      else loop scope
  in loop scope

let scope_expr (m : expr) : expr = 
  let rec aux scope m =
    match m with
    | Int i -> Int i
    | Bool b -> Bool b
    | Unit -> Unit
    | UOpr (opr, m) -> UOpr (opr, aux scope m)
    | BOpr (opr, m, n) -> 
      let m = aux scope m in
      let n = aux scope n in
      BOpr (opr, m, n)
    | Var s -> 
      (match find_var scope s with
       | None -> raise (UnboundVariable s)
       | Some x -> Var x)
    | Fun (f, x, m) -> 
      let fvar = new_var f in
      let xvar = new_var x in
      let m = aux ((f, fvar) :: (x, xvar) :: scope) m in
      Fun (fvar, xvar, m)
    | App (m, n) ->
      let m = aux scope m in
      let n = aux scope n in
      App (m, n)
    | Let (x, m, n) ->
      let xvar = new_var x in
      let m = aux scope m in
      let n = aux ((x, xvar) :: scope) n in
      Let (xvar, m, n)
    | Seq (m, n) ->
      let m = aux scope m in
      let n = aux scope n in
      Seq (m, n)
    | Ifte (m, n1, n2) ->
      let m = aux scope m in
      let n1 = aux scope n1 in
      let n2 = aux scope n2 in
      Ifte (m, n1, n2)
    | Trace m -> Trace (aux scope m)
  in
  aux [] m

(* ------------------------------------------------------------ *)

(* parser for the high-level language *)

let parse_prog (s : string) : expr =
  match string_parse (whitespaces >> parse_expr ()) s with
  | Some (m, []) -> scope_expr m
  | _ -> raise SyntaxError

let rec compile_expr scope = function
  | Int i -> string_concat_list ["Push "; string_of_int i; "; "]
  | Bool b -> string_concat_list ["Push "; if b then "True" else "False"; "; "]
  | Unit -> "Push Unit; "
  | UOpr (Neg, m) -> string_append (compile_expr scope m) "Neg; "
  | UOpr (Not, m) -> string_append (compile_expr scope m) "Not; "
  | BOpr (Add, m1, m2) -> string_concat_list [compile_expr scope m1; compile_expr scope m2; "Swap; Add; "]
  | BOpr (Sub, m1, m2) -> string_concat_list [compile_expr scope m1; compile_expr scope m2; "Swap; Sub; "]
  | BOpr (Mul, m1, m2) -> string_concat_list [compile_expr scope m1; compile_expr scope m2; "Swap; Mul; "]
  | BOpr (Div, m1, m2) -> string_concat_list [compile_expr scope m1; compile_expr scope m2; "Swap; Div; "]
  | BOpr (Mod, m1, m2) -> compile_mod scope m1 m2
  | BOpr (And, m1, m2) -> string_concat_list [compile_expr scope m1; compile_expr scope m2; "And; "]
  | BOpr (Or, m1, m2) -> string_concat_list [compile_expr scope m1; compile_expr scope m2; "Or; "]
  | BOpr (Lt, m1, m2) -> string_concat_list [compile_expr scope m1; compile_expr scope m2; "Swap; Lt; "]
  | BOpr (Gt, m1, m2) -> string_concat_list [compile_expr scope m1; compile_expr scope m2; "Swap; Gt; "]
  | BOpr (Lte, m1, m2) -> string_concat_list [compile_expr scope (BOpr (Gt, m1, m2)); "Not; "]
  | BOpr (Gte, m1, m2) -> string_concat_list [compile_expr scope (BOpr (Lt, m1, m2)); "Not; "]
  | BOpr (Eq, m1, m2) -> compile_eq scope m1 m2
  | Var x ->
    (match find_var scope x with
      | None -> raise (UnboundVariable x)
      | Some v -> string_concat_list ["Push "; v; "; Lookup; "])
  | Fun (f, x, m) -> compile_fun scope f x m
  | App (m1, m2) -> string_concat_list [compile_expr scope m1; compile_expr scope m2; "Swap; "; "Call; "]
  | Let (x, m1, m2) -> compile_let scope x m1 m2
  | Seq (m1, m2) -> string_concat_list [compile_expr scope m1; "Pop; "; compile_expr scope m2]
  | Ifte (m, n1, n2) -> compile_ifte scope m n1 n2
  | Trace m -> string_append (compile_expr scope m) "Trace; "
  | _ -> failwith "Not implemented yet"

and compile_mod scope m1 m2 =
  let cm1 = compile_expr scope m1 in
  let cm2 = compile_expr scope m2 in
  (* m1/m2 *)
  let divide = compile_expr scope (BOpr (Div, m1, m2)) in
  (* m1 - (m1/m2) * m2 *)
  string_concat_list [divide; cm2; "Mul; "; cm1; "Sub; "]

and compile_eq scope m1 m2 = (* equivalent to NOT (Lt or Gt) *)
  let less_than = compile_expr scope (BOpr (Lt, m1, m2)) in
  let great_than = compile_expr scope (BOpr (Gt, m1, m2)) in
  string_concat_list [less_than; "Not; "; great_than; "Not; "; "And; "]

and compile_fun scope f x m =
  let fv = new_var f in                   (* new variable name for function *)
  let f_scope = (f, fv) :: scope in       (* add f to scope *)
  let xv = new_var x in                   (* new variable name for parameter *)
  let x_f_scope = (x, xv) :: f_scope in   (* add x to scope *)
  let body = compile_expr x_f_scope m in  (* compile function with updated scope *)
  string_concat_list ["Push "; fv; "; Fun Push "; xv; "; Bind; "; body; "Swap; Return; End; "]
  
and compile_let scope x m n =
  let cm = compile_expr scope m in    (* compile expression *)
  let xv = new_var x in               (* new variable name for the bound *)
  let x_scope = (x, xv) :: scope in   (* add bound to scope *)
  let cn = compile_expr x_scope n in  (* compile expression with updated scope *)
  string_concat_list [cm; "Push "; xv; "; "; "Bind; "; cn]

and compile_ifte scope m n1 n2 =
  let _if = compile_expr scope m in     (* compile if expression *)
  let _then = compile_expr scope n1 in  (* compile then expression *)
  let _else = compile_expr scope n2 in  (* compile else expression *)
  (* Construct the if-then-else logic using stack commands *)
  string_concat_list [_if; "If "; _then; "Else "; _else; "End; "]
  

let compile (s : string) : string = (* YOUR CODE *)
  compile_expr [] (scope_expr (parse_prog s))

let test_string = "let rec pi n =
  let q = 1 in
  let r = 180 in
  let t = 60 in
  let j = 2 in
  let rec loop n q r t j =
  if n > 0 then
  let u = 3 * (3 * j + 1) * (3 * j + 2) in
  let y = (q * (27 * j - 12) + 5 * r) / (5 * t) in
  trace y;
  let q' = 10 * q * j * (2 * j - 1) in
  let r' = 10 * u * (q * (5 * j - 2) + r - y * t) in
  let t' = t * u in
  let j' = j + 1 in
  loop (n - 1) q' r' t' j'
  else ()
  in
  loop n q r t j
  in
  pi 6"

let () = print_string(compile(test_string))