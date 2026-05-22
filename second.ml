open Gooderis


let () = assert (fact 5 = 120)

let () = assert (mymap (function x -> x * x) [1; 2; 3; 5] = [ 1; 4; 9; 25 ])

let ssw = Compose (South, Compose (South, West))

let () = print_endline (string_of_direction ssw)

let e1 = Divide (Plus (Integer 2, Integer 1), Integer 3)

let () = print_endline (string_of_expr e1)
