(* Part 2: Parsing expressions *)
let rec parse_Expr toks =
  let t = lookahead toks in
  match t with
  | Some Tok_Let -> parse_LetExpr toks
  | Some Tok_If -> parse_IfExpr toks
  | Some Tok_Fun -> parse_FunctionExpr toks
  | _ -> parse_OrExpr toks

and parse_LetExpr toks =
  let t = lookahead toks in
  match t with
  | Some Tok_Let -> 
    let toks2 = (match_token toks Tok_Let) in

    let recursive = lookahead toks2 in
    let toks3 = 
    match recursive with 
    | Some Tok_Rec -> (match_token toks2 Tok_Rec) 
    | None -> toks2 
    | _ -> toks2

  in

    let id = lookahead toks3 in
    let toks4 = (match_token toks3 (Option.get id)) in
    let toks5 = (match_token toks4 Tok_Equal) in
    let (toks6, expr1) = (parse_Expr toks5) in
    let toks7 = (match_token toks6 Tok_In) in
    let (toks8, expr2) = (parse_Expr toks7) in
    (*FIX boolean in rec statement*)
    (toks8, Let((match id with | Some Tok_ID x -> x | _ -> ""), (match recursive with | Some Tok_Rec -> true | _ -> false), expr1, expr2))
  | _ -> raise (InvalidInputException "parse_LetExpr")

and parse_FunctionExpr toks =
  let t = lookahead toks in
  match t with
  | Some Tok_Fun ->
    let toks2 = (match_token toks Tok_Fun) in
    let id = lookahead toks2 in
    let toks3 = (match_token toks2 (Option.get id)) in
    let toks4 = (match_token toks3 Tok_Arrow) in 
    let (toks5, expr) = (parse_Expr toks4) in
    
    (toks5, Fun((match id with | Some Tok_ID x -> x | _ -> ""), expr))
  | _ -> raise (InvalidInputException "parse_FunctionExpr")

and parse_IfExpr toks = 
  let t = lookahead toks in
  match t with
  | Some Tok_If -> 
    let toks2 = (match_token toks Tok_If) in
    let (toks3, expr1) = (parse_Expr toks2) in
    let toks4 = (match_token toks3 Tok_Then) in
    let (toks5, expr2) = (parse_Expr toks4) in
    let toks6 = (match_token toks5 Tok_Else) in
    let (toks7, expr3) = (parse_Expr toks6) in
    (toks7, If(expr1,expr2,expr3))
  | _ -> raise (InvalidInputException "parse_IfExpr")

and parse_OrExpr toks =
  let (toks2, e1) = (parse_AndExpr toks) in
  let t = lookahead toks2 in
  match t with
  | Some Tok_Or -> 
    let toks3 = (match_token toks2 Tok_Or) in 
    let (toks4, e2) = (parse_OrExpr toks3) in 
    (toks4, Binop(Or,e1,e2))
  | _ -> (toks2, e1)

and parse_AndExpr toks = 
  let (toks2, e1) = (parse_EqualityExpr toks) in
  let t = lookahead toks2 in
  match t with
  | Some Tok_And -> 
    let toks3 = (match_token toks2 Tok_And) in 
    let (toks4, e2) = (parse_AndExpr toks3) in 
    (toks4, Binop(And,e1,e2))
  | _ -> (toks2, e1)

and parse_EqualityExpr toks =
  let (toks2, e1) = (parse_RelationalExpr toks) in 
  let t = lookahead toks2 in
  match t with
  | Some Tok_Equal -> 
    let toks3 = (match_token toks2 Tok_Equal) in
    let (toks4, e2) = (parse_EqualityExpr toks3) in
    (toks4, Binop(Equal, e1,e2))
  | Some Tok_NotEqual ->
    let toks3 = (match_token toks2 Tok_NotEqual) in
    let (toks4, e2) = (parse_EqualityExpr toks3) in
    (toks4, Binop(NotEqual, e1,e2))
  | _ -> (toks2, e1)

and parse_RelationalExpr toks =
  let (toks2, e1) = (parse_AdditiveExpr toks) in 
  let t = lookahead toks2 in
  match t with
  | Some Tok_Less -> 
    let toks3 = (match_token toks2 Tok_Less) in
    let (toks4, e2) = (parse_RelationalExpr toks3) in
    (toks4, Binop(Less, e1,e2))
  | Some Tok_Greater ->
    let toks3 = (match_token toks2 Tok_Greater) in
    let (toks4, e2) = (parse_RelationalExpr toks3) in
    (toks4, Binop(Greater, e1,e2))
  | Some Tok_LessEqual -> 
    let toks3 = (match_token toks2 Tok_LessEqual) in
    let (toks4, e2) = (parse_RelationalExpr toks3) in
    (toks4, Binop(LessEqual, e1,e2))
  | Some Tok_GreaterEqual ->
    let toks3 = (match_token toks2 Tok_GreaterEqual) in
    let (toks4, e2) = (parse_RelationalExpr toks3) in
    (toks4, Binop(GreaterEqual, e1,e2))
  | _ -> (toks2, e1)

and parse_AdditiveExpr toks =
  let (toks2, e1) = (parse_MultiplicativeExpr toks) in 
  let t = lookahead toks2 in
  match t with
  | Some Tok_Add -> 
    let toks3 = (match_token toks2 Tok_Add) in
    let (toks4, e2) = (parse_AdditiveExpr toks3) in
    (toks4, Binop(Add, e1,e2))
  | Some Tok_Sub ->
    let toks3 = (match_token toks2 Tok_Sub) in
    let (toks4, e2) = (parse_AdditiveExpr toks3) in
    (toks4, Binop(Sub, e1,e2))
  | _ -> (toks2, e1)

and parse_MultiplicativeExpr toks = 
  let (toks2, e1) = (parse_ConcatExpr toks) in 
  let t = lookahead toks2 in
  match t with
  | Some Tok_Mult -> 
    let toks3 = (match_token toks2 Tok_Mult) in
    let (toks4, e2) = (parse_MultiplicativeExpr toks3) in
    (toks4, Binop(Mult, e1,e2))
  | Some Tok_Div ->
    let toks3 = (match_token toks2 Tok_Div) in
    let (toks4, e2) = (parse_MultiplicativeExpr toks3) in
    (toks4, Binop(Div, e1,e2))
  | _ -> (toks2, e1)

and parse_ConcatExpr toks =
  let (toks2, e1) = (parse_UnaryExpr toks) in
  let t = lookahead toks2 in
  match t with
  | Some Tok_Concat -> 
    let toks3 = (match_token toks2 Tok_Concat) in 
    let (toks4, e2) = (parse_ConcatExpr toks3) in 
    (toks4, Binop(Concat, e1,e2))
  | _ -> (toks2, e1)

and parse_UnaryExpr toks = 
  let t = lookahead toks in
  match t with
  | Some Tok_Not -> 
    let toks2 = (match_token toks Tok_Not) in 
    let (toks3, expr) = (parse_UnaryExpr toks2) in
    (toks3, Not(expr))
  | _ -> 
    let (toks2, expr) = (parse_FunctionCallExpr toks) in
    (toks2, expr)

and parse_FunctionCallExpr toks =
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
    let (toks3, expr) = (parse_Expr toks2) in
    let toks4 = (match_token toks3 Tok_RParen) in
    (toks4, (expr))
  | _ -> raise (InvalidInputException "parse_PrimaryExpr")

let rec parse_expr toks = parse_Expr toks

(* parse_expr function *)
(* returns a tuple of (list of remaining tokens, parsed expr) *)


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
    let (toks5, expr) = (parse_Expr toks4) in
    let toks6 = (match_token toks5 Tok_DoubleSemi) in
    (toks6, Def((match id with | Some Tok_ID x -> x | _ -> ""), expr))
  | _ -> raise (InvalidInputException "parse_PrimaryExpr")

and parse_ExprMutop toks = 
  let (toks2, expr) = (parse_expr toks) in
  let t = lookahead toks2 in
  match t with
  | Some Tok_DoubleSemi ->
    (toks2, Expr(expr))
  | _ -> raise (InvalidInputException "parse_PrimaryExpr")

let rec parse_mutop toks = 
  let (toks2, mutop) = parse_Mutop toks in
  ([], mutop)