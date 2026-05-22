open Cs3110
(*
   fold_left (0 - 1) [2; 3]
  fold_left ((0 - 1) - 2) [3]
  fold_left (((0 - 1) - 2) - 3) []
*)

let () = assert (fold_left ( - ) 0 [ 1; 2; 3 ] = -6)

(* let () = Printf.printf "%d" b *)

(*
   fold_right [1; 2] (3 - 0)
  fold_right [1] (2 - (3 - 0))
  fold_right [] (1 - (2 - (3 - 0)))
*)

(* let () = assert (fold_right (-) [1; 2; 3] 0 = 2) *)
