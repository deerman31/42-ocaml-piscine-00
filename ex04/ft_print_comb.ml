let is_ascending n =
  let digit1 = n / 100 in
  let digit2 = n / 10 mod 10 in
  let digit3 = n mod 10 in
  digit1 < digit2 && digit2 < digit3

(* let rec range n1 n2 = if n1 > n2 then [] else n1 :: range (n1 + 1) n2 *)
let range n1 n2 =
  let rec loop acc n = if n > n2 then acc else loop (n :: acc) (n + 1) in
  loop [] n1

(* let rec filter p lst =
   match lst with
   | [] -> []
   | first :: rest -> if p first then first :: filter p rest else filter p rest *)
let filter p lst =
  let rec loop acc lst =
    match lst with
    | [] -> acc
    | first :: rest ->
        if p first then loop (first :: acc) rest else loop acc rest
  in
  loop [] lst

let ft_print_comb () =
  let start_num = 12 in
  let end_num = 789 in
  let lst = filter is_ascending (range start_num end_num) in
  let rec print_lst lst =
    match lst with
    | [] -> print_string "\n"
    | first :: rest ->
        if first / 100 = 0 then print_int 0;
        print_int first;
        if first != end_num then print_string ",";
        print_lst rest
  in
  print_lst lst

let test () = ft_print_comb ()
let () = test ()
