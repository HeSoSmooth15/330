open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = match lookahead toks with
  | Some Tok_Let -> let tok = match_token toks Tok_Let in parse_let tok
  | Some Tok_If -> let tok = match_token toks Tok_If in parse_if tok
  | Some Tok_Fun -> let tok = match_token toks Tok_Fun in parse_function tok
  | _ -> parse_or toks

and parse_let toks = let (lst,result) = parse_rec toks in let id = lookahead lst in match id with
  | Some Tok_ID id -> (let first = match_token lst (Tok_ID id) in match lookahead first with
    | Some Tok_Equal -> (let second = match_token first Tok_Equal in let (new_lst1, expr1) = parse_expr second in match lookahead new_lst1 with
      | Some Tok_In -> let third = match_token new_lst1 Tok_In in let (new_lst2, expr2) = parse_expr third in (new_lst2, Let (id, result, expr1, expr2))
      | _ -> raise (InvalidInputException "Tok_In error"))
    | _ -> raise (InvalidInputException "Tok_equal error"))
  | _ -> raise (InvalidInputException "Tok_ID error")



and parse_rec toks = match lookahead toks with
  | Some Tok_Rec -> (match_token toks Tok_Rec,true)
  | _ -> (toks,false)

and parse_if toks = let (tok1, expr) = parse_expr toks in match lookahead tok1 with
  | Some Tok_Then -> (let tok2 = match_token tok1 Tok_Then in let (tok3, expr1) = parse_expr tok2 in match lookahead tok3 with
    | Some Tok_Else -> let tok4 = match_token tok3 Tok_Else in let (tok5, expr2) = parse_expr tok4 in (tok5, If (expr, expr1, expr2))
    | _ -> raise (InvalidInputException "Tok_then error"))
  | _ -> raise (InvalidInputException "Tok_else error")

(* and parse_function toks =
  let t = lookahead toks in
  match t with
  | Some Tok_Fun ->
    let toks2 = (match_token toks Tok_Fun) in
    let id = lookahead toks2 in
    let toks3 = (match_token toks2 (Option.get id)) in
    let toks4 = (match_token toks3 Tok_Arrow) in 
    let (toks5, expr) = (parse_expr toks4) in
    
    (toks5, Fun((match id with | Some Tok_ID x -> x | _ -> ""), expr))
  | _ -> raise (InvalidInputException "parse_FunctionExpr") *)

and parse_function toks = let tok = lookahead toks in match tok with
  | Some Tok_ID tok -> (let tok2 = match_token toks (Tok_ID tok) in match lookahead tok2 with
    | Some Tok_Arrow -> let tok3 = match_token tok2 Tok_Arrow in let (tok4,expr) = parse_expr tok3 in (tok4, Fun (tok, expr))
    | _ -> raise (InvalidInputException "parse_fun error"))
  | _ -> raise (InvalidInputException "parse_fun error")

  
and parse_or toks = let (tok, andExpr) = parse_And toks in match lookahead tok with
  | Some Tok_Or -> let tok2 = match_token tok Tok_Or in let (tok3, orExpr) = parse_or tok2 in (tok3, Binop (Or,andExpr,orExpr))
  | _ -> tok, andExpr

and parse_And toks = let (tok, parseEqual) = parse_Equality toks in match lookahead tok with
  | Some Tok_And -> let tok2 =  match_token toks Tok_And in let (tok3, parseAnd)= parse_And tok2 in (tok3, Binop (And, parseEqual, parseAnd))
  | _ -> tok, parseEqual

and parse_Equality toks =
  let (toks2, e1) = (parse_Relational toks) in 
  let t = lookahead toks2 in
  match t with
  | Some Tok_Equal -> 
    let toks3 = (match_token toks2 Tok_Equal) in
    let (toks4, e2) = (parse_Equality toks3) in
    (toks4, Binop(Equal, e1,e2))
  | Some Tok_NotEqual ->
    let toks3 = (match_token toks2 Tok_NotEqual) in
    let (toks4, e2) = (parse_Equality toks3) in
    (toks4, Binop(NotEqual, e1,e2))
  | _ -> (toks2, e1)

and parse_Relational toks = let (toks2, e1) = (parse_Additive toks) in let t = lookahead toks2 in
  match t with
  | Some Tok_Less -> 
    let toks3 = (match_token toks2 Tok_Less) in
    let (toks4, e2) = (parse_Relational toks3) in
    (toks4, Binop(Less, e1,e2))
  | Some Tok_Greater ->
    let toks3 = (match_token toks2 Tok_Greater) in
    let (toks4, e2) = (parse_Relational toks3) in
    (toks4, Binop(Greater, e1,e2))
  | Some Tok_LessEqual -> 
    let toks3 = (match_token toks2 Tok_LessEqual) in
    let (toks4, e2) = (parse_Relational toks3) in
    (toks4, Binop(LessEqual, e1,e2))
  | Some Tok_GreaterEqual ->
    let toks3 = (match_token toks2 Tok_GreaterEqual) in
    let (toks4, e2) = (parse_Relational toks3) in
    (toks4, Binop(GreaterEqual, e1,e2))
  | _ -> (toks2, e1)

and parse_Additive toks = let (toks2, e1) = (parse_Multiplicative toks) in let t = lookahead toks2 in
  match t with
  | Some Tok_Add -> 
    let toks3 = (match_token toks2 Tok_Add) in
    let (toks4, e2) = (parse_Additive toks3) in
    (toks4, Binop(Add, e1,e2))
  | Some Tok_Sub ->
    let toks3 = (match_token toks2 Tok_Sub) in
    let (toks4, e2) = (parse_Additive toks3) in
    (toks4, Binop(Sub, e1,e2))
  | _ -> (toks2, e1)

and parse_Multiplicative toks = let (toks2, e1) = (parse_concat toks) in let t = lookahead toks2 in
  match t with
  | Some Tok_Mult -> 
    let toks3 = (match_token toks2 Tok_Mult) in
    let (toks4, e2) = (parse_Multiplicative toks3) in
    (toks4, Binop(Mult, e1,e2))
  | Some Tok_Div ->
    let toks3 = (match_token toks2 Tok_Div) in
    let (toks4, e2) = (parse_Multiplicative toks3) in
    (toks4, Binop(Div, e1,e2))
  | _ -> (toks2, e1)

and parse_concat toks = let (toks2, e1) = (parse_Unary toks) in let t = lookahead toks2 in
  match t with
  | Some Tok_Concat -> 
    let toks3 = (match_token toks2 Tok_Concat) in 
    let (toks4, e2) = (parse_concat toks3) in 
    (toks4, Binop(Concat, e1,e2))
  | _ -> (toks2, e1)


and parse_Unary toks = let t = lookahead toks in
  match t with
  | Some Tok_Not -> 
    let toks2 = (match_token toks Tok_Not) in 
    let (toks3, expr) = (parse_Unary toks2) in
    (toks3, Not(expr))
  | _ -> 
    let (toks2, expr) = (parse_function_call toks) in
    (toks2, expr)

and parse_function_call toks =
  let (toks2, e1) = (parse_PrimaryExpr toks) in
  let t = lookahead toks2 in
  match t with
  | Some Tok_Int x ->
    let (toks3, e2) = (parse_PrimaryExpr toks2) in
    (toks3, FunctionCall(e1,e2))
  | Some Tok_Bool x ->
    let (toks3, e2) = (parse_PrimaryExpr toks2) in
    (toks3, FunctionCall(e1,e2))
  | Some Tok_String x ->
    let (toks3, e2) = (parse_PrimaryExpr toks2) in
    (toks3, FunctionCall(e1,e2))
  | Some Tok_ID x ->
    let (toks3, e2) = (parse_PrimaryExpr toks2) in
    (toks3, FunctionCall(e1,e2))
  | Some Tok_LParen ->
    let (toks3, e2) = (parse_PrimaryExpr toks2) in
    (toks3, FunctionCall(e1,e2))
  | _ -> (toks2, e1)

and parse_PrimaryExpr toks =
  let t = lookahead toks in
  match t with
  | Some Tok_Int x -> 
    let toks2 = (match_token toks (Tok_Int (x))) in
    (toks2, Value(Int(x)))
  | Some Tok_Bool x -> 
    let toks2 = (match_token toks (Tok_Bool (x))) in
    (toks2, Value(Bool(x)))
  | Some Tok_String x -> 
    let toks2 = (match_token toks (Tok_String (x))) in
    (toks2, Value(String(x)))
  | Some Tok_ID x -> 
    let toks2 = (match_token toks (Tok_ID (x))) in
    (toks2, ID(x))
  | Some Tok_LParen -> 
    let toks2 = (match_token toks Tok_LParen) in
    let (toks3, expr) = (parse_expr toks2) in
    let toks4 = (match_token toks3 Tok_RParen) in
    (toks4, (expr))
  | _ -> raise (InvalidInputException "parse_PrimaryExpr")

let rec parse_Expr toks = parse_expr toks






(* Part 3: Parsing mutop *)

let rec parse_Mutop toks = 
let t = lookahead toks in
  match t with 
  | Some Tok_DoubleSemi -> ([], NoOp)
  | Some Tok_Def  -> parse_DefMutop toks
  | _ -> parse_ExprMutop toks

and parse_DefMutop toks = 
  let t = lookahead toks in
  match t with 
  | Some Tok_Def -> 
    let toks2 = (match_token toks Tok_Def) in
    let id = lookahead toks2 in
    let toks3 = (match_token toks2 (Option.get id)) in
    let toks4 = (match_token toks3 Tok_Equal) in
    let (toks5, expr) = (parse_expr toks4) in
    let toks6 = (match_token toks5 Tok_DoubleSemi) in
    (toks6, Def((match id with | Some Tok_ID x -> x | _ -> ""), expr))
  | _ -> raise (InvalidInputException "parse_PrimaryExpr")

and parse_ExprMutop toks = 
  let (toks2, expr) = (parse_Expr toks) in
  let t = lookahead toks2 in
  match t with
  | Some Tok_DoubleSemi ->
    (toks2, Expr(expr))
  | _ -> raise (InvalidInputException "parse_PrimaryExpr")

let rec parse_mutop toks = 
  let (toks2, mutop) = parse_Mutop toks in
  ([], mutop)