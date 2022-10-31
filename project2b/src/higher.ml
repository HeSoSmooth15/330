open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = fold (fun a x -> if x = e then true else a)false lst;;

let is_present lst x = map (fun y -> if y = x then 1 else 0)lst;;

let count_occ lst target = fold (fun a y -> if y = target then a+1 else a)0 lst;;

let uniq lst = fold (fun a x -> if contains_elem a x then a else x::a) [] lst;;

let assoc_list lst = fold (fun a x -> (x, count_occ lst x)::a) [] (uniq lst);;

let ap fns args = fold (fun a lst -> a@(map lst args)) [] fns
