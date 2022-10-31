open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []


(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
 List.fold_left (fun acc x -> match x with (location, transition, destination)-> if mem destination acc = false && mem location qs
   && transition = s then destination::acc else acc) [] nfa.delta

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
List.fold_left (fun acc x -> match x with (loc, trans, dest) -> if trans = None && mem loc acc then
     insert dest acc else acc) qs nfa.delta

let rec accept_aux (nfa: ('q,char) nfa_t) (char: char list) (qs: 'q list) : bool = match char with
  | [] -> if (length qs = 0) then false else ((mem (hd qs) nfa.fs) || (accept_aux nfa [] (tl qs)))
  | first::[] -> accept_aux nfa [] (e_closure nfa (move nfa (e_closure nfa qs) (Some first)))
  | h::t -> accept_aux nfa t (move nfa (e_closure nfa qs) (Some h))

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =accept_aux nfa (explode s) [nfa.q0]
  

(* let rec accept_aux (nfa: ('q,'s) nfa_t) (qs: 'q list)  (character: char list): 'q list = match character with
  | [] -> qs(*if mem des nfa.fs = true else false *)
  | h::t -> let rec inside_func (nfa_delta_lst: ('q, 's) transition list): ('q, 's) transition list = match nfa_delta_lst with 
    | [] -> []
    | first::rest -> match first with (loc, trans, des) -> match trans with 
      | None -> []
      | Some value -> if h = value && check_is_empty move nfa [loc] trans then accept_aux nfa move nfa [loc] trans t 
        else if h != value then inside_func rest else []

let rec check_is_empty (qs: 'q list) : bool = if length qs = 0 then false else true  *)


(* let rec accept_aux (nfa: ('q,'s) nfa_t) (nfa_delta_lst: ('q, 's) transition list) (character: char list) (destin: 'q) (return: bool) : bool = match character with
  | [] -> if mem des nfa.fs = true else false
  | h::t -> let rec ins (nfa_delta: ('q, 's) transition list) : ('q, 's) transition list = match nfa_delta with
    | [] -> false
    | heads::tails -> match heads with (loc, trans, des) -> match trans with
      | None -> false
      | Some value -> if h = value then accept_aux nfa nfa.delta t check_for_transition nfa_delta_lst nfa h trans des else if h != value then 
      ins tails else false

let rec check_for_transition *)

(* let rec check_for_transition (nfa: ('q,'s) nfa_t) (nfa_list: ('q, 's) transition list) (c: char) (s: 's option) (q: 'q): bool = match nfa_list with
  | [] -> false
  | first::rest -> match first with (location, transition, destination) -> match transition with
    | None -> false
    | Some v -> if v = c && location = q then true 
      else if v != c && location != q then check_for_transition rest c s q else if nfa.q0 = location then true else false *)

(* let rec accept_aux (nfa: ('q,char) nfa_t) (nfa_list: ('q, 's) transition list) (character: char list) : bool = match nfa_list with 
  | [] -> false
  | first::rest -> match first with (location, transition, destination) -> match (character) with 
     | [] -> false (*match nfa.fs with | value::junk -> if value = destination then true else false | [] -> false *)
    | h::t -> if look_for_transition nfa h transition destination = true then accept_aux nfa rest t else false

let rec look_for_transition (nfa: ('q,char) nfa_t) (c: char) (s: 's option) (q: 'q): bool = match nfa.delta with
  | [] -> false
  | first::rest -> match first with (location, transition, destination) -> match transition with
    | None -> false
    | Some v -> if v = c && location = q then true 
      else if v != c && location != q then look_for_transition nfa c s q else if nfa.q0 = location then true else false *)
(*******************************)
(* Part 2: Subset Construction *)
(*******************************)
let dfa_state (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's): 'q list = e_closure nfa (move nfa qs (Some s))

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  List.fold_left(fun acc x ->  (e_closure nfa (move nfa qs (Some x)))::acc)[] nfa.sigma



let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.fold_left(fun acc x -> insert (qs, Some x , (e_closure nfa (move nfa qs (Some x)))) acc)[] nfa.sigma


let rec new_finals_helper states qs =
match states with
  |[] -> []
  |h :: t -> if (elem h qs) then [qs] else (new_finals_helper t qs)


let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
   (new_finals_helper nfa.fs qs)

let helper_2 (nfa: ('q,'s) nfa_t) (work: 'q list list) : 'q list list =
  fold_left (fun acc x -> if (exists (fun y -> mem y nfa.fs) x) then x::acc else acc) [] work
  
let helper_aux (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t) (work: 'q list list) (set: 'q list) : ('q list, 's) nfa_t =
  let element = new_states nfa set in let fortitude = new_trans nfa set in
    if not (List.for_all (fun x -> mem x (union work dfa.qs)) element) then
       {sigma = nfa.sigma; qs = (union element dfa.qs); q0 = dfa.q0; fs = dfa.fs; delta = (union fortitude dfa.delta);} else
         {sigma = nfa.sigma; qs = dfa.qs; q0 = dfa.q0; fs = dfa.fs; delta = (union fortitude dfa.delta);}



let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t = match dfa.qs with
  | [] -> {sigma = nfa.sigma; qs = work; q0 = dfa.q0; fs = helper_2 nfa work; delta = dfa.delta;}
  | h::t -> if (mem h work) then nfa_to_dfa_step nfa {sigma = nfa.sigma; qs = t; q0 = dfa.q0; fs = dfa.fs; delta = dfa.delta;} work
      else nfa_to_dfa_step nfa (helper_aux nfa dfa work h) (h::work)

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  nfa_to_dfa_step nfa {sigma = nfa.sigma; qs = [e_closure nfa [nfa.q0]]; q0 = e_closure nfa [nfa.q0]; fs = []; delta = [];} []
