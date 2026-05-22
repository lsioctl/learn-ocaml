let rec fold_left fn acc = function
  | [] -> acc
  | h :: t -> fold_left fn (fn acc h) t


(*
  This is wrong, it is not a real fold right,
  I have to read more to get it right :D  
*)

(* let rec fold_right fn list acc =
  match list with
    | [] -> acc
    | h :: t -> fold_right fn t (fn h acc) *)