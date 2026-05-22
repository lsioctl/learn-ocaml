let rec fact = function
    | 0 -> 1
    | n -> n * fact (n - 1)

let mymap fn list =
    let rec aux acc = function
        | [] -> []
        | [x] -> fn x :: acc
        | h :: t -> fn h :: aux acc t

    in
    aux [] list

(* site's solution *)
let rec mymap2 fn list =
    if List.length list > 0 then
        (fn (List.hd list)) :: (mymap2 fn (List.tl list))
    else
        []

let avg list = match list with
    | [] -> 0.0
    | _ -> let sum = List.fold_left (+) 0 list in
        (float sum) /. (float (List.length list))

(* site's solution *)
let avg2 list =
    if List.length list > 0 then
        let sum = List.fold_left (+) 0 list in
        (float sum) /. (float (List.length list))
    else
        0.0

(* Sum types *)

type direction =
 | North
 | South
 | East
 | West
 | Compose of direction * direction


let rec string_of_direction = function
  | North -> "North"
  | South -> "South"
  | East -> "East"
  | West -> "West"
  | Compose (d1, d2) -> (string_of_direction d1) ^ "--" ^ (string_of_direction d2)


type expr =
  | Plus of expr * expr
  | Minus of expr * expr
  | Divide of expr * expr
  | Multiply of expr * expr
  | Integer of int

let rec string_of_expr = function
  | Plus (l, r) -> (string_of_expr l) ^ " Plus " ^(string_of_expr r)
  | Minus (l, r) -> (string_of_expr l) ^ " Minus " ^(string_of_expr r)
  | Divide (l, r) -> (string_of_expr l) ^ " Divide " ^(string_of_expr r)
  | Multiply (l, r) -> (string_of_expr l) ^ " Multiply " ^(string_of_expr r)
  | Integer n -> string_of_int n
