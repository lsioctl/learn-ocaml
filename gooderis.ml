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

  (*
    precedence not respected, so this has to be a
    correctly parsed expression
    TODO: maybe I'm mixing up parsing and evaluating ?
  *)   
let rec eval = function
  | Plus (l, r) -> (eval l) + (eval r)
  | Minus (l, r) -> (eval l) - (eval r)
  | Divide (l, r) -> (eval l) / (eval r)
  | Multiply (l, r) -> (eval l) * (eval r)
  | Integer n -> n

(* generics *)

type 'a genexpr =
  | PlusGen of 'a genexpr * 'a genexpr
  | MinusGen of 'a genexpr * 'a genexpr
  | DivideGen of 'a genexpr * 'a genexpr
  | MultiplyGen of 'a genexpr * 'a genexpr
  | Value of 'a

type 'a operators = {
  add: 'a -> 'a -> 'a ;
  minus: 'a -> 'a -> 'a ;
  divide: 'a -> 'a -> 'a ;
  multiply: 'a -> 'a -> 'a ;
}

let rec gen_eval op = function
  | PlusGen (l, r) -> op.add (gen_eval op l) (gen_eval op r)
  | MinusGen (l, r) -> op.minus (gen_eval op l) (gen_eval op r)
  | DivideGen (l, r) -> op.divide (gen_eval op l) (gen_eval op r)
  | MultiplyGen (l, r) -> op.multiply (gen_eval op l) (gen_eval op r)
  | Value n -> n

let float_ops = {
  (* spaces are vital to avoid false comment for float multiplication *)
  divide = ( /. );
  multiply = ( *. );
  add = ( +. );
  minus = ( -. );
}
