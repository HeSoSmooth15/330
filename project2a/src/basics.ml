(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = match tup with (a, b ,c) -> (c, b, a);;

let is_odd x = if x mod 2 != 0 then true else false;;

let area x y = match x with (x1, x2) -> match y with (y1, y2) -> abs((y2-x2)*(y1-x1)) ;;

let volume x y = match x with (x1, x2, x3) -> match y with (y1, y2, y3) -> abs((y2-x2)*(y1-x1)*(y3-x3)) ;;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = if n = 0 then 0 else if n = 1 then 1 else fibonacci (n-1) + fibonacci (n-2);;

let rec pow x y = if y = 0 then 1 
    else x * pow x (y - 1);;

let rec log x y = if x >= y then 1
else 1 + log x (y/x);;


let rec gcf x y = if y == 0 then x
    else gcf y (x mod y);;

let rec prime x y = match y with
    | 1 -> true    
    | _ -> (x mod y <> 0) && prime x (y-1);;

let rec is_prime x = if x > 1 then match x with
    | 0 | 1 -> false
    | _ -> prime x (x-1) else false;;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = let rec length lst =
     match lst with
      | [] -> 0
      | first::rest -> 1 + length rest in
	
	if idx >= length lst then
		failwith "Out of bounds"
	else
		match lst with
		| [] -> failwith "Out of bounds"
		| first::rest ->
			if idx = 0 then
				first
			else
				get (idx -1) rest;;

let larger lst1 lst2 =
let rec length lst =
     match lst with
      | [] -> 0
      | first::rest -> 1 + length rest in
 

      if length lst1 > length lst2 then lst1
      else if length lst1 < length lst2 then lst2
      else [];;

let reverse lst = let rec append l m = match l with
        | [] -> m
        | first::rest -> first::(append rest m) in
    let rec rev l = match l with
        | [] -> []
        | first::rest -> append (rev rest) (first::[]) in

    rev lst;;

let rec combine lst1 lst2 = match lst1 with
    | [] -> lst2
    | first::rest -> first::(combine rest lst2);;

let rec sort l =
   match l with
     [] -> []
   | h::t -> ins h (sort t)
 and ins x l =
   match l with
     [] -> [x]
   | h::t -> if x <= h then x::l else h::ins x t;;
let rec merge lst1 lst2 = sort (combine lst1 lst2);;
let rot lst = match lst with
    | [] -> []
    | (first::rest) -> rest@[first];;
let rec rotate shift lst = match shift with
    | 0 -> lst
    | _ -> rotate (shift - 1) (rot lst);;

let rec is_palindrome lst = if (reverse lst) = lst then true else false;;
