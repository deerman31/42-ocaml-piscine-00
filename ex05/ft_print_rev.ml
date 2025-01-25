let ft_print_rev s =
  let rec helper i =
    if i < 0 then print_char '\n'
    else (
      print_char s.[i];
      helper (i - 1))
  in
  helper (String.length s - 1)

let () =
  ft_print_rev "42Tokyo";
  ft_print_rev "0123456789";
  ft_print_rev ""
