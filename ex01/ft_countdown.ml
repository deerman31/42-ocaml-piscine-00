(*
let rec ft_countdown x =
   let print_int_endline x =
     print_int x;
     print_char '\n'
   in
   if x <= 0 then print_int_endline 0
   else (
     print_int_endline x;
     ft_countdown (x - 1))
 *)

let ft_countdown x =
  let print_int_endline x =
    print_int x;
    print_char '\n'
  in
  let rec loop acc =
    if acc <= 0 then print_int_endline 0
    else (
      print_int_endline acc;
      loop (acc - 1))
  in
  loop x

let () =
  ft_countdown 3;
  print_char '\n';
  ft_countdown 0;
  print_char '\n';
  ft_countdown (-1)
