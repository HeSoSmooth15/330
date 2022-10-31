open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let opt_helper x = match x with
  | Some value -> value
  | None -> failwith "No Value!"

let rec int_mem x t =  match t with
   | IntLeaf -> false
   | IntNode (first, second, third, fourth, fifth) -> 
   if x < first then int_mem x third else
     match second with
       | None -> if x = first then true else false
       | Some value -> if x = first then true else if value > x 
       then int_mem x fourth else
         if value < x then int_mem x fifth else true

let rec int_insert x t = if int_mem x t then t else match t with
     | IntLeaf -> IntNode(x, None, IntLeaf, IntLeaf, IntLeaf)
     | IntNode (first, second, third, fourth, fifth) -> match second with
       | None -> if x < first then IntNode (x, Some first, third, fourth, fifth) 
       else IntNode (first, Some x, third, fourth, fifth)
       | Some value -> if x < first then IntNode (first, second, int_insert x third, fourth, fifth) 
       else if value > x 
       then IntNode(first, second, third, int_insert x fourth, fifth) else IntNode(first, second, third, fourth, int_insert x  fifth)

let rec int_size t = match t with
   | IntLeaf -> 0
   | IntNode (first, second, third, fourth, fifth) -> match second with
     | None -> 1 + int_size third + int_size fourth + int_size fifth
     | Some value -> 2 + int_size third + int_size fourth + int_size fifth

let rec int_max t = match t with
   | IntLeaf -> failwith "int_max"
   | IntNode (first, second, third, fourth, fifth) -> match fifth with
     | IntNode (f, g, h, i, j) -> int_max fifth
     | IntLeaf -> match second with
      | None -> first
      | Some value -> value

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_contains k t = match t with
   | MapLeaf -> false
   | MapNode ((first, temp), second, third, fourth, fifth) -> if k < first then map_contains k third else if k = first 
   then true else match second with
     | None -> false
     | Some (m, n) -> if k > m then map_contains k fifth else if k = m then true else map_contains k fourth

let rec map_get k t =
  if (map_contains k t) then match t with
   | MapLeaf -> failwith "map_get"
   | MapNode ((first, temp), second, third, fourth, fifth) -> if k < first then map_get k third else if k > first then match second with
     | None -> failwith "map_get"
     | Some (m, n) -> if k < m then map_get k fourth else if k = m then n else map_get k fifth
     else temp else failwith "map_get"

let rec map_put k value t = 
  if map_contains k t then failwith "map_put" else match t with
     | MapLeaf -> MapNode((k, value), None, MapLeaf, MapLeaf, MapLeaf)
     | MapNode ((first, temp), second, third, fourth, fifth) -> match second with
       | None -> if k < first then MapNode((k, value), Some (first, temp), third, fourth, fifth) 
       else MapNode((first, temp), Some (k, value), third, fourth, fifth)
       | Some (m, n) -> if k < first then MapNode((first, temp), second, map_put k value third, fourth, fifth) 
       else if m > k then MapNode((first, temp), second, third, map_put k value fourth, fifth) 
       else MapNode((first, temp), second, third, fourth, map_put k value fifth)


(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)

type lookup_table = (string * int) list list

let empty_table : lookup_table = []

let push_scope (table : lookup_table) : lookup_table = ["end",100]::table

let pop_scope (table : lookup_table) : lookup_table = match table with
  | [] -> failwith "No scopes remain!"
  | _::rest -> rest

let add_var name value (table : lookup_table) : lookup_table = match table with
  | [] -> failwith "There are no scopes to add a variable to!"
  | first::rest -> match first with
    | [] -> failwith "There are no scopes to add a variable to!"
    | first1::rest1 -> ([(name,value)]@first1::rest1)::rest

let rec lookup name (table : lookup_table) = match table with
  | [] -> failwith "Variable not found!"
  | first::rest -> let rec helper lst w = match lst with
    | [] -> failwith "Variable not found!"
    | first1::rest1 -> let (w, var) = first1 in if w = name then var else helper rest1 name
    in helper first name
