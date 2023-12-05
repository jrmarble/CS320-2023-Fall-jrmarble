#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

Constants:
⟨digit⟩ ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
⟨nat⟩ ::= ⟨digit⟩ | ⟨digit⟩⟨nat⟩
⟨int⟩ ::= ⟨nat⟩ | -⟨nat⟩
⟨bool⟩ ::= True | False
⟨char⟩ ::= a | b | ... | z
⟨sym⟩ ::= ⟨char⟩ | ⟨sym⟩⟨char⟩ | ⟨sym⟩⟨digit⟩
⟨const⟩ ::= ⟨int⟩ | ⟨bool⟩ | Unit | ⟨sym⟩

Programs:
⟨prog⟩ ::= ⟨coms⟩
⟨com⟩ ::= Push ⟨const⟩ | Pop | Swap | Trace
   | Add | Sub | Mul | Div
   | And | Or | Not
   | Lt | Gt
   | If ⟨coms⟩ Else ⟨coms⟩ End
   | Bind | Lookup
   | Fun ⟨coms⟩ End | Call | Return
⟨coms⟩ ::= ϵ | ⟨com⟩; ⟨coms⟩

*)

type const =
  | Int of int
  | Bool of bool
  | Unit
  | Sym of string

type com =
  | Push of const
  | Pop
  | Swap
  | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt
  | If of com list * com list  (* If-Else End *)
  | Bind | Lookup
  | Fun of com list            (* Function definition *)
  | Call | Return

type closure = {
  name: string;
  env: (string * value) list;
  body: com list;
}

and value =
   | Const of const
   | Closure of closure

type coms = com list
 

(* parsing constants *)
let parse_nat = 
   let* n = natural << whitespaces in pure n
 let parse_int =
   (let* n = parse_nat in pure (Int n)) <|>
   (keyword "-" >> let* n = parse_nat in pure (Int (-n)))
 let parse_bool =
   (keyword "True" >> pure (Bool true)) <|>
   (keyword "False" >> pure (Bool false))
 let parse_unit =
   keyword "Unit" >> pure Unit
let parse_char =
   satisfy (fun c -> ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
let parse_sym =
   let* c1 = parse_char in
   let* c_rest = many (parse_char <|> digit) in
   let char_list = c1 :: c_rest in
   let string_builder fwork =
      let rec helper cs =
         match cs with
            | [] -> ()
            | ch :: rest ->
            fwork ch; helper rest in
      helper char_list in
   pure (string_make_fwork string_builder)
let parse_const =
   parse_int <|>
   parse_bool <|>
   parse_unit <|>
   (parse_sym >>= fun s -> pure (Sym s))

(* parsing commands *)
let rec parse_com () =
   parse_if_else () <|>
   (keyword "Push" >> parse_const >>= fun c -> pure (Push c)) <|>
   (keyword "Pop" >> pure Pop) <|>
   (keyword "Swap" >> pure Swap) <|>
   (keyword "Trace" >> pure Trace) <|>
   (keyword "Add" >> pure Add) <|>
   (keyword "Sub" >> pure Sub) <|>
   (keyword "Mul" >> pure Mul) <|>
   (keyword "Div" >> pure Div) <|>
   (keyword "And" >> pure And) <|>
   (keyword "Or" >> pure Or) <|>
   (keyword "Not" >> pure Not) <|>
   (keyword "Lt" >> pure Lt) <|>
   (keyword "Gt" >> pure Gt) <|>
   (keyword "Bind" >> pure Bind) <|>
   (keyword "Lookup" >> pure Lookup) <|>
   (keyword "Fun" >> parse_coms () << keyword "End" >>= fun cmds -> pure (Fun cmds)) <|>
   (keyword "Call" >> pure Call) <|>
   (keyword "Return" >> pure Return)

and parse_if_else () =
   let* _ = keyword "If" in
   let* c1 = parse_coms () in
   let* _ = keyword "Else" in
   let* c2 = parse_coms () in
   let* _ = keyword "End" in
   pure (If (c1, c2))
   
and parse_coms () = many (parse_com () << keyword ";")
    
(* interpreter *)

let rec eval (stack : value list) (trace : string list) (env : (string * value) list) (prog : coms) : string list =
   match prog with
   | [] -> trace  (* termination returns trace *)
   | Push c :: p0 (* push stack *) -> eval ((Const c) :: stack) trace env p0
   | Pop :: p0 -> (
       match stack with
       | _ :: s0  (* PopStack *) -> eval s0 trace env p0
       | []       (* PopError *) -> "Panic" :: trace)
   | Swap :: p0 -> (
       match stack with
       | a :: b :: s0 (* SwapStack *) -> eval (b :: a :: s0) trace env p0
       | _            (* SwapError *) -> "Panic" :: trace)  
   | Trace :: p0 -> (
       match stack with
       | Const c :: s0 (* TraceStack *) -> eval (Const Unit :: s0) (toString c :: trace) env p0
       | _             (* TraceError *) -> "Panic" :: trace)  
   (* Add, Sub, Mul, Div *)
   | Add :: p0 -> (
      match stack with
      | Const (Int i) :: Const (Int j) :: s0 (* AddStack *)  -> eval (Const (Int (i + j)) :: s0) trace env p0
      | _ :: _ :: s0                         (* AddError1 *) -> "Panic" :: trace
      | []                                   (* AddError2 *) -> "Panic" :: trace
      | _ :: []                              (* AddError3 *) -> "Panic" :: trace)
   | Sub :: p0 -> (
      match stack with
      | Const (Int i) :: Const (Int j) :: s0 (* SubStack *)  -> eval (Const (Int (i - j)) :: s0) trace env p0
      | _ :: _ :: s0                         (* SubError1 *) -> "Panic" :: trace
      | []                                   (* SubError2 *) -> "Panic" :: trace
      | _ :: []                              (* SubError3 *) -> "Panic" :: trace)
   | Mul :: p0 -> (
      match stack with
      | Const (Int i) :: Const (Int j) :: s0 (* MulStack *)  -> eval (Const (Int (i * j)) :: s0) trace env p0
      | _ :: _ :: s0                         (* MulError1 *) -> "Panic" :: trace
      | []                                   (* MulError2 *) -> "Panic" :: trace
      | _ :: []                              (* MulError3 *) -> "Panic" :: trace)
   | Div :: p0 -> (
      match stack with
      | Const (Int i) :: Const (Int 0) :: s0 (* DivError0 *) -> "Panic" :: trace
      | Const (Int i) :: Const (Int j) :: s0 (* DivStack *)  -> eval (Const (Int (i / j)) :: s0) trace env p0
      | _ :: _ :: s0                         (* DivError1 *) -> "Panic" :: trace
      | []                                   (* DivError2 *) -> "Panic" :: trace
      | _ :: []                              (* DivError3 *) -> "Panic" :: trace)
   (* And, Or, Not *)
   | And :: p0 -> (
      match stack with
      | Const (Bool a) :: Const (Bool b) :: s0 (* AndStack *)  -> eval (Const (Bool (a && b)) :: s0) trace env p0
      | _ :: _ :: s0                           (* AndError1 *) -> "Panic" :: trace
      | []                                     (* AndError2 *) -> "Panic" :: trace
      | _ :: []                                (* AndError3 *) -> "Panic" :: trace)
   | Or :: p0 -> (
      match stack with
      | Const (Bool a) :: Const (Bool b) :: s0 (* OrStack *)  -> eval (Const (Bool (a || b)) :: s0) trace env p0
      | _ :: _ :: s0                           (* OrError1 *) -> "Panic" :: trace
      | []                                     (* OrError2 *) -> "Panic" :: trace
      | _ :: []                                (* OrError3 *) -> "Panic" :: trace)
   | Not :: p0 -> (
      match stack with
      | Const (Bool a) :: s0 (* NotStack *)  -> eval (Const (Bool (not a)) :: s0) trace env p0
      | _ :: s0              (* NotError1 *) -> "Panic" :: trace
      | []                   (* NotError2 *) -> "Panic" :: trace)
   (* Lt, Gt *)
   | Lt :: p0 -> (
      match stack with
      | Const (Int i) :: Const (Int j) :: s0 (* LtStack *)  -> eval (Const (Bool (i < j)) :: s0) trace env p0
      | _ :: _ :: s0                         (* LtError1 *) -> "Panic" :: trace
      | []                                   (* LtError2 *) -> "Panic" :: trace
      | _ :: []                              (* LtError3 *) -> "Panic" :: trace)
   | Gt :: p0 -> (
      match stack with
      | Const (Int i) :: Const (Int j) :: s0 (* GtStack *)  -> eval (Const (Bool (i > j)) :: s0) trace env p0
      | _ :: _ :: s0                         (* GtError1 *) -> "Panic" :: trace
      | []                                   (* GtError2 *) -> "Panic" :: trace
      | _ :: []                              (* GtError3 *) -> "Panic" :: trace)
   (* If then Else *)
   | If (c1, c2) :: p0 -> (
      match stack with
      | Const (Bool true) :: s0  (* IfStack *)      -> eval s0 trace env (c1 @ p0)
      | Const (Bool false) :: s0 (* ElseStack *)    -> eval s0 trace env (c2 @ p0)
      | _ :: s0                  (* IfElseError1 *) -> "Panic" :: trace
      | []                       (* IfElseError2 *) -> "Panic" :: trace)
   (* Bind, Lookup *)
   | Bind :: p0 -> (
      match stack with
      | Const (Sym x) :: v :: s0 (* BindStack *)  -> eval s0 trace ((x, v) :: env) p0
      | _ :: _ :: s0             (* BindError1 *) -> "Panic" :: trace 
      | []                       (* BindError2 *) -> "Panic" :: trace
      | _ :: []                  (* BindError3 *) -> "Panic" :: trace)
   | Lookup :: p0 -> (
      match stack with
      | Const (Sym x) :: s0 -> (
          match lookFor x env with  (* use helper lookFor to seach through env *)
          | Some v (* LookupStack *)  -> eval (v :: s0) trace env p0
          | None   (* LookupError3 *) -> "Panic" :: trace)  
      | _ :: s0    (* LookupError1 *) -> "Panic" :: trace
      | []         (* LookupError2 *) -> "Panic" :: trace)
   (* Function, Call, Return *)
   | Fun cmds :: p0 -> (
      match stack with
      | Const (Sym f) :: s0 -> (* FunStack *)
        let closure = { name = f; env = env; body = cmds } in
        eval (Closure closure :: s0) trace env p0
      | _ :: s0 (* FunError1 *) -> "Panic" :: trace 
      | []      (* FunError2 *) -> "Panic" :: trace)

   | Call :: p0 -> (
      match stack with
      | Closure { name = f; env = closure_env; body = body_cmds } :: arg :: s0 -> (* CallStack *)
         let new_env: (string * value) list = (f, Closure { name = f; env = closure_env; body = body_cmds }) :: closure_env in
         eval (arg :: Closure { name = "cc"; env = env; body = p0 } :: s0) trace new_env body_cmds
      | _ :: _ :: s0 (* CallError1 *) -> "Panic" :: trace 
      | []           (* CallError2 *) -> "Panic" :: trace 
      | _ :: []      (* CallError3 *) -> "Panic" :: trace)


   | Return :: p0 -> (
      match stack with
      | Closure { name = _; env = closure_env; body = body_cmds } :: arg :: s0 -> (* RetStack *)
        eval (arg :: s0) trace closure_env body_cmds
      | _ :: _ :: s0 (* ReturnError1 *) -> "Panic" :: trace 
      | []           (* ReturnError2 *) -> "Panic" :: trace 
      | _ :: []      (* ReturnError3 *) -> "Panic" :: trace)
   
 
 and toString (c : const) : string =
   match c with
   | Int i -> string_of_int i
   | Bool b -> string_of_bool b
   | Unit -> "Unit"
   | Sym s -> s

and lookFor (sym: string) (env: (string * value) list) : value option =
   match env with
   | [] -> None
   | (s, v) :: rest ->
       if s = sym then Some v
       else lookFor sym rest

let interp (s : string) : string list option =
   match string_parse (whitespaces >> parse_coms ()) s with
   | Some (prog, []) -> Some (eval [] [] [] prog)
   | _ -> None

let test (t: int) (input: string) (expected_out: string list option) : unit =
   let actual_out = interp input in
   if actual_out = expected_out then
      print_string("Test %d passed.\n") 
   else
      print_string("Test %d failed.\n")
    
let tests () : unit =
   let test_cases = [
      (1, "Push 3; Push 5; Add;", Some ["8"]);
      (2, "Push True; Not;", Some ["False"]);
      (3, "Push 3; Push 4; Lt;", Some ["True"]);
      (4, "Push 2; Push 0; Div;", Some ["Panic"]);
      (5, "Push x; Lookup;", None);
      (6, "Push 3; Push 3; Mul; Push -4; Push 3; Mul; Add; Push 7; Add; Trace;", Some["4"])
      (7, "Push 2; Push 2; Mul; Push 3; Push 3; Mul; Gt; Trace;", Some["True"])
   ] in
   List.iter (fun (id, input, expected) -> test id input expected) test_cases
    
(* Run all tests *)
let () = tests ()