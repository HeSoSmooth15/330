open TokenTypes
open Str
(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let ex_lparen = Str.regexp "(" 
let ex_rparen = Str.regexp ")" 
let ex_equal = Str.regexp "=" 
let ex_notequal = Str.regexp "<>" 
let ex_greater = Str.regexp ">" 
let ex_less = Str.regexp "<" 
let ex_greaterequal = Str.regexp ">=" 
let ex_lessequal = Str.regexp "<=" 

let ex_or = Str.regexp "||" 
let ex_and = Str.regexp "&&" 

let ex_not = Str.regexp "not" 
let ex_if = Str.regexp "if" 
let ex_then = Str.regexp "then" 
let ex_else = Str.regexp "else" 

let ex_add = Str.regexp "+" 
let ex_sub = Str.regexp "-" 
let ex_mult = Str.regexp "*" 
let ex_div = Str.regexp "/" 
let ex_concat = Str.regexp "\\^" 

let ex_let = Str.regexp "let" 
let ex_rec = Str.regexp "rec" 
let ex_in = Str.regexp "in" 
let ex_def = Str.regexp "def" 
let ex_fun = Str.regexp "fun" 
let ex_arrow = Str.regexp "->"

let ex_doublesemi = Str.regexp ";;" 

(* tokens with complex rules *)
let ex_bool = Str.regexp "true\\|false" 
let ex_int = Str.regexp "[0-9]+" 
let ex_negint = Str.regexp "(-[0-9]+)" 
let ex_string = Str.regexp "\"[^\"]*\"" 
let ex_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" 

let ex_skip = Str.regexp "[ \t\n]*" 
let ex_extra = Str.regexp "[a-zA-Z0-9]+" 

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)
let tokenize input = 
  let rec next_token str pos = 
    if pos >= (String.length str) then 
      []
              
    else 
      (* Negative Int *)  
      if (Str.string_match ex_negint str pos) then 
        let token = Str.matched_string str in 
        (Tok_Int (int_of_string (String.sub token 1 ((String.length token)-2))))::(next_token str (Str.match_end()))
  
      (* Tok_LParen *)
      else if (Str.string_match ex_lparen str pos) then 
        (Tok_LParen)::(next_token str (pos+1))
      
      (* Tok_RParen *)
      else if (Str.string_match ex_rparen str pos) then 
        (Tok_RParen)::(next_token str (pos+1))
        
      (* match the longer operators first *)
      (* Tok_Equal *)
      else if (Str.string_match ex_equal str pos) then 
        (Tok_Equal)::(next_token str (pos+2))
        
      (* Tok_NotEqual *)
      else if (Str.string_match ex_notequal str pos) then 
        (Tok_NotEqual)::(next_token str (pos+2))  

        (* Tok_Greater *)
      else if (Str.string_match ex_greater str pos) then 
        (Tok_Greater)::(next_token str (pos+1))
        
      (* Tok_Less *)
      else if (Str.string_match ex_less str pos) then 
        (Tok_Less)::(next_token str (pos+1))

      (* Tok_GreaterEqual *)  
      else if (Str.string_match ex_greaterequal str pos) then 
        (Tok_GreaterEqual)::(next_token str (pos+2))
      
      (* Tok_LessEqual *) 
      else if (Str.string_match ex_lessequal str pos) then 
        (Tok_LessEqual)::(next_token str (pos+2)) 

      (* Tok_DoubleSemi *)  
      else if (Str.string_match ex_doublesemi str pos) then 
        (Tok_DoubleSemi)::(next_token str (pos+2))  
      (* now match the one char operators *)
        
      (* Tok_Or *)
      else if (Str.string_match ex_or str pos) then 
        (Tok_Or)::(next_token str (pos+2))
        
      (* Tok_And *)
      else if (Str.string_match ex_and str pos) then 
        (Tok_And)::(next_token str (pos+2))

      (* Tok_Not *)
      else if (Str.string_match ex_not str pos) then 
        (Tok_Not)::(next_token str (pos+2))

      (* match for keywords *)
      (* if there are chars/ints after then match tok_id *)
      (* Tok_If *)
      else if (Str.string_match ex_if str pos) 
        then let token_key = Str.matched_string str in
        let new_pos = Str.match_end() in
        
        if (Str.string_match ex_extra str new_pos)
          then let token_id = Str.matched_string str in
          (Tok_ID (token_key^token_id))::(next_token str (Str.match_end()))
        else (Tok_If)::(next_token str new_pos)
      
      (* Tok_Then *)
      else if (Str.string_match ex_then str pos) 
        then let token_key = Str.matched_string str in
        let new_pos = Str.match_end() in
        
        if (Str.string_match ex_extra str new_pos)
          then let token_id = Str.matched_string str in
          (Tok_ID (token_key^token_id))::(next_token str (Str.match_end()))
        else (Tok_Then)::(next_token str new_pos)

      (* Tok_Else *)
      else if (Str.string_match ex_else str pos) 
        then let token_key = Str.matched_string str in
        let new_pos = Str.match_end() in
        
        if (Str.string_match ex_extra str new_pos)
          then let token_id = Str.matched_string str in
          (Tok_ID (token_key^token_id))::(next_token str (Str.match_end()))
        else (Tok_Else)::(next_token str new_pos)
      
      (* Tok_Let *)
      else if (Str.string_match ex_let str pos) 
        then let token_key = Str.matched_string str in
        let new_pos = Str.match_end() in
        
        if (Str.string_match ex_extra str new_pos)
          then let token_id = Str.matched_string str in
          (Tok_ID (token_key^token_id))::(next_token str (Str.match_end()))
        else (Tok_Let)::(next_token str new_pos)

      (* Tok_Rec *)
      else if (Str.string_match ex_rec str pos) 
        then (Tok_Rec)::(next_token str (pos+3))
      (* Tok_In *)
      else if (Str.string_match ex_in str pos) 
        then (Tok_In)::(next_token str (pos+2))
      (* Tok_Def *)
      else if (Str.string_match ex_def str pos) 
        then (Tok_Def)::(next_token str (pos+3))    
      (* Tok_Fun *)
      else if (Str.string_match ex_fun str pos) 
        then (Tok_Fun)::(next_token str (pos+3)) 
      (* now process the urnary operators *)
      
      (* Tok_Arrow *)
      else if (Str.string_match ex_arrow str pos) 
        then (Tok_Arrow)::(next_token str (pos+2))

      (* Tok_Add *)
      else if (Str.string_match ex_add str pos) 
        then (Tok_Add)::(next_token str (pos+1))
        
      (* Tok_Sub *)
      else if (Str.string_match ex_sub str pos) 
        then (Tok_Sub)::(next_token str (pos+1))
        
      (* Tok_Mult *)
      else if (Str.string_match ex_mult str pos) 
        then (Tok_Mult)::(next_token str (pos+1))
        
      (* Tok_Div *)
      else if (Str.string_match ex_div str pos) 
        then (Tok_Div)::(next_token str (pos+1))
      
      (* Tok_Concat *)
      else if (Str.string_match ex_concat str pos) 
        then (Tok_Concat)::(next_token str (pos+1))
         
      
      (* finish by processing complex tokens *)
      
      (* Tok_Bool *)
      else if (Str.string_match ex_bool str pos)
        then let token = Str.matched_string str in
        (Tok_Bool (bool_of_string token))::(next_token str (Str.match_end()))

      (* Tok_Int *) 
      else if (Str.string_match ex_int str pos) then 
        let token = Str.matched_string str in 
        (Tok_Int (int_of_string token))::(next_token str (Str.match_end()))
        
      (* Tok_ID *)          
      (* id also checked after keywords too *)
      else if (Str.string_match ex_id str pos)
        then let token = Str.matched_string str in 
        (Tok_ID token)::(next_token str (Str.match_end()))

      (* Tok_String *)  
      (* id also checked after keywords too *)
      else if (Str.string_match ex_string str pos)
        then let token = Str.matched_string str in 
        (Tok_String (String.sub token 1 ((String.length token) - 2)))::(next_token str (Str.match_end()))
      

      (* Whitespace, Tab, Newline check *)
      else if (Str.string_match ex_skip str pos)
        then (next_token str (Str.match_end()))

      else 
        raise (InvalidInputException "tokenize")
  in    
next_token input 0
