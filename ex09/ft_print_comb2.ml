let rev lst =
  let rec loop acc lst =
    match lst with [] -> acc | first :: rest -> loop (first :: acc) rest
  in
  loop [] lst

let ft_print_comb2 () =
  let n1_max = 98 in
  let n2_max = 99 in
  let rec loop n1 n2 acc =
    if n1 > n1_max then rev acc
    else if n2 > n2_max then loop (n1 + 1) (n1 + 2) acc
    else loop n1 (n2 + 1) ((n1, n2) :: acc)
  in
  let rec print_lst lst =
    let print_num_helper n =
      if n < 10 then (
        print_char '0';
        print_int n)
      else print_int n
    in

    match lst with
    | [] -> print_char '\n'
    | (n1, n2) :: rest ->
        print_num_helper n1;
        print_char ' ';
        print_num_helper n2;
        if n1 != n1_max then (
          print_char ',';
          print_char ' ');
        print_lst rest
  in
  print_lst (loop 0 1 [])

let test () = ft_print_comb2 ()
let () = test ()
