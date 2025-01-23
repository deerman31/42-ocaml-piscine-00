let is_tolower c = 'a' <= c && 'z' >= c
let is_toupper c = 'A' <= c && 'Z' >= c

let ft_rot_n n str =
  let small_z_num = int_of_char 'z' in
  let big_z_num = int_of_char 'Z' in
  let small_a_num = int_of_char 'a' in
  let big_a_num = int_of_char 'A' in

  let rotate_char c =
    let c_num = int_of_char c in
    if is_tolower c && c_num + n > small_z_num then
      small_a_num + ((c_num + n) mod small_z_num) - 1
    else if is_toupper c && c_num + n > big_z_num then
      big_a_num + ((c_num + n) mod big_z_num) - 1
    else c_num + n
  in

  let alphabet_rot c =
    if is_tolower c || is_toupper c then char_of_int (rotate_char c) else c
  in
  String.map alphabet_rot str

let test () =
  print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_n 42 "0123456789");

  print_endline (ft_rot_n 2 "OI2EAS67B9");

  print_endline (ft_rot_n 0 "Damned !");
  print_endline (ft_rot_n 42 "");
  print_endline (ft_rot_n 1 "NBzlk qnbjr !")

let () = test ()
