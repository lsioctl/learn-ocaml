open Gooderis

let () = assert (fact 5 = 120)

let () =
  assert (
    mymap
      (function
        | x -> x * x)
      [ 1; 2; 3; 5 ]
    = [ 1; 4; 9; 25 ])
;;

let () =
  assert (
    mymap_exercice
      (function
        | x -> x * x)
      [ 1; 2; 3; 5 ]
    = [ 1; 4; 9; 25 ])
;;

let ssw = Compose (South, Compose (South, West))
let () = print_endline (string_of_direction ssw)
let e1 = Divide (Plus (Integer 2, Integer 1), Integer 3)
let () = print_endline (string_of_expr e1)
let () = print_endline (string_of_int (eval e1))
let e2 = DivideGen (PlusGen (Value 2., Value 1.), Value 3.)
let () = print_endline (string_of_float (gen_eval float_ops e2))

let () =
  assert (
    mymap_exercice
      (function
        | x -> x * x)
      [ 1; 2; 3; 5 ]
    = [ 1; 4; 9; 25 ])
;;

let mat1 = [| [| 0; 1; 2 |]; [| 1; 5; 10 |]; [| 2; 10; 8 |] |]
let () = assert (is_sym_imper mat1 = true)
