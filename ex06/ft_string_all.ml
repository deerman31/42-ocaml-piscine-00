(* let rev lst =
     let rec loop acc lst = match lst with
     | [] -> acc
     | first::rest-> loop (first::acc) rest in loop [] lst

   let filter p lst =
     let rec loop acc lst =
       match lst with
       | [] -> rev acc
       | first :: rest ->
           if p first then loop (first :: acc) rest else loop acc rest
     in
     loop [] lst *)

let ft_string_all p str =
  let rec loop i =
    if i < 0 then true else if not (p str.[i]) then false else loop (i - 1)
  in
  loop (String.length str - 1)

let () =
  let is_digit c = c >= '0' && c <= '9' in
  if ft_string_all is_digit "0123456789" then print_endline "OK"
  else print_endline "NG";
  if ft_string_all is_digit "" then print_endline "OK" else print_endline "NG";
  if ft_string_all is_digit "E0123456789" then print_endline "OK"
  else print_endline "NG"
