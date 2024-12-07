let ft_is_palindrome str =
  let rec loop i e =
    if i >= e then true
    else if str.[i] != str.[e] then false
    else loop (i + 1) (e - 1)
  in
  loop 0 (String.length str - 1)


let test () =
  if ft_is_palindrome "0123456789" then print_endline "OK"
  else print_endline "NG";
  if ft_is_palindrome "radar" then print_endline "OK"
  else print_endline "NG";
  if ft_is_palindrome "madam" then print_endline "OK"
  else print_endline "NG";
  if ft_is_palindrome "car" then print_endline "OK"
  else print_endline "NG";
  if ft_is_palindrome "" then print_endline "OK"
  else print_endline "NG"

let () = test ()
