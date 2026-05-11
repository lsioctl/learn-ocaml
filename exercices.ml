
let rec facto x = match x with
    | 0 -> 1
    | n -> n * facto (n - 1)


let rec facto2 x = if x < 1 then 1 else x * facto2 (x - 1)


let rec tail = function
    | [ ] -> None
    | [x] -> Some x
    | _ :: t -> tail t


let rec tail_2 = function
    | [ ] -> None
    | [_] -> None
    | [x; y] -> Some(x, y)
    | _ :: t -> tail_2 t


let rec get_n n = function
    | [ ] -> None
    | h :: t ->
        (* exemple of print debug inline *)
        (* let () = Printf.printf "%d %d\n" n h in *)
        if n = 0 then Some h else get_n (n - 1) t

let rec length = function
    | [ ] -> 0
    | [_] -> 1
    | _ :: t -> 1 + length t


let rec reverse = function
    | [ ] -> [ ]
    | [ x ] -> [ x ]
    | h :: t -> reverse t @ [ h ]


let palindrome = function
    | [ ] -> true
    | [ _ ] -> true
    | l -> if reverse l = l then true else false

(* this one is from the solution, quicker, cleaner *)
let palindrome2 list = list = reverse list


type 'a node =
    | One of 'a 
    | Many of 'a node list





let my_flatten list = 
    let rec aux acc = function
        | [ ] -> acc
        | One h :: t -> aux (acc @ [ h ]) t 
        | Many h :: t -> aux (aux acc h) t
    in
    aux [] list

(* this is from the solution *)
let flatten list =
    let rec aux acc = function
      | [] -> acc
      | One x :: t -> aux (x :: acc) t
      | Many l :: t -> aux (aux acc l) t
    in
    List.rev (aux [] list);;

let rec deduplicate_debug = function
    | a :: (b :: _ as t) ->
        let _ = print_string a in
        if a = b then deduplicate_debug t else a :: deduplicate_debug t
    | l ->
        let _ = print_string("bouhhh")
    in l

let rec deduplicate = function
    | a :: (b :: _ as t) -> if a = b then deduplicate t else a :: deduplicate t
    | list_of_one_elt -> list_of_one_elt

let stack_push h l = h :: l

let stack_pop = function
    | [] -> (None, [])
    | h :: t -> (Some h, t)


(** Careful, adding an element at the end of a list is anti pattern as
    it imposes a full copy
    see:
    https://stackoverflow.com/questions/6732524/what-is-the-easiest-way-to-add-an-element-to-the-end-of-the-list
    The reason there's not a standard function to do this is that appending at 
    the end of a list is an anti-pattern (aka a "snoc list" or a Schlemiel the Painter algorithm). 
    Adding an element at the end of a list requires a full copy of the list.
    Adding an element at the front of the list requires allocating a single cell—the tail of 
    the new list can just point to the old list.

    Also:

    https://stackoverflow.com/questions/49051006/append-an-element-at-the-end-of-a-list
*)
let pack list =
    let rec aux current acc = function
        | [] -> []
        | [ x ] -> acc @ [current @ [x]]
        | a :: (b :: _ as t) ->
            if a = b then aux (current @ [a]) acc t
            else aux [] (acc @ [current @ [a]]) t
        in
        aux [] [] list
        
let pack2 list =
    let rec aux current acc = function
      | [] -> []    (* Can only be reached if original list is empty *)
      | [x] -> (x :: current) :: acc
      | a :: (b :: _ as t) ->
         if a = b then aux (a :: current) acc t
         else aux [] ((a :: current) :: acc) t
    in
    List.rev (aux [] [] list);;

let encode1 list =
    let rec aux count acc = function
        | [] -> [] (* original list empty*)
        | [x] -> (count + 1, x) :: acc
        | a :: (b :: _ as t) ->
            if a = b then aux (count + 1) acc t
            else aux 0 ((count + 1, a) :: acc) t
    in
    (* Now I get why we doing it reverse :) *)
    List.rev (aux 0 [] list)

type 'a rle = 
    | One of 'a
    | Many of int * 'a


let encode2 list =
    let rec aux count acc = function
        | [] -> [] (* original list empty*)
        | [x] -> 
            if count = 0 then (One x) :: acc
            else Many (count + 1, x) :: acc
        | a :: (b :: _ as t) ->
            if a = b then 
                let _ = Printf.printf "equals %s %s %d\n" a b count
            in
            aux (count + 1) acc t
            else
                let _ = Printf.printf "not equals %s %s %d\n" a b count
            in
                if count = 0 then aux 0 ((One a) :: acc) t
                else aux 0 ((Many (count + 1, a) :: acc)) t
    in
    List.rev (aux 0 [] list)


let encode3 list =
    let create_rle cnt elem =
        if cnt = 0 then One elem
        else Many (cnt + 1, elem)
    in
    let rec aux count acc = function
        | [] -> [] (* original list empty*)
        | [x] -> (create_rle count x) :: acc
        | a :: (b :: _ as t) ->
            if a = b then aux (count + 1) acc t
            else aux 0 ((create_rle count a) :: acc) t
    in
    List.rev (aux 0 [] list)


let duplicate list = 
    let rec aux acc = function
    | [] -> acc
    | [x] -> x :: x :: acc
    | h :: t -> h :: h :: aux acc t
    in
    aux [] list
