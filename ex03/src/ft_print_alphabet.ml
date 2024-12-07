(* let rec range n1 n2 = if n1 > n2 then [] else n1 :: range (n1 + 1) n2 *)
let range n1 n2 =
  let rec loop acc n = if n > n2 then acc else loop (n :: acc) (n + 1) in
  loop [] n1

(* let rec map f lst = match lst with [] -> [] | first :: rest -> f first :: rest *)
let map f lst =
  let rec loop acc lst =
    match lst with [] -> acc | first :: rest -> loop (f first :: acc) rest
  in
  loop [] lst

let ft_print_alphabet () =
  let start_num = int_of_char 'a' in
  let end_num = int_of_char 'z' in
  let lst = map char_of_int (range start_num end_num) in
  let rec ft_print lst =
    match lst with
    | [] -> print_char '\n'
    | first :: rest ->
        print_char first;
        ft_print rest
  in
  ft_print lst

let test () = ft_print_alphabet ()
let () = test ()
