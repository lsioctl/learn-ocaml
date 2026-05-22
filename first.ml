open Exercices

let () = assert (facto 5 = 120)

let () = assert (facto2 5 = 120)

let () = assert (tail [1; 2; 3] = Some 3)

let () = assert (tail [1] = Some 1)

let () = assert (tail [] = None)

let () = assert (tail_2 [1; 2; 3] = Some (2, 3))

let () = assert (tail_2 ["a"; "b"; "c"; "z"] = Some ("c", "z"))

let () = assert (tail_2 [] = None)

let () = assert (tail_2 [1] = None)

(* let () =
    match get_n 1 [1; 2; 3] with
    | None -> print_endline "Nooooone"
    | Some x -> Printf.printf "%d\n" x *)

let _ = assert (get_n 1 [1; 2; 3] = Some 2)

let _ = assert (length [1; 2; 3] = 3)

let () = assert (reverse [1; 2; 3] = [3; 2; 1])

let _ = assert (palindrome [("a", "b", "b", "a")] = true)

let _ =
  assert (
    my_flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]]
    = ["a"; "b"; "c"; "d"; "e"] )

let _ =
  assert (
    deduplicate
      ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
    = ["a"; "b"; "c"; "a"; "d"; "e"] )

let _ = assert (stack_push 1 [2; 3] = [1; 2; 3])

let _ = assert (stack_pop [1; 2; 3] = (Some 1, [2; 3]))

let _ =
  assert (
    pack
      ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
    = [ ["a"; "a"; "a"; "a"]
      ; ["b"]
      ; ["c"; "c"]
      ; ["a"; "a"]
      ; ["d"; "d"]
      ; ["e"; "e"; "e"; "e"] ] )

let _ =
  assert (
    encode1
      ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
    = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] )

let _ =
  assert (
    encode3
      ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
    = [ Many (4, "a")
      ; One "b"
      ; Many (2, "c")
      ; Many (2, "a")
      ; One "d"
      ; Many (4, "e") ] )

let _ = assert (duplicate [1; 2; 3] = [1; 1; 2; 2; 3; 3])
