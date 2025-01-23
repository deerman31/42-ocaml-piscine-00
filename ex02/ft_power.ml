(* let rec ft_power n1 n2 = if n2 < 1 then 1 else n1 * ft_power n1 (n2 - 1) *)

let ft_power n1 n2 =
  let rec loop acc base =
    if base < 1 then acc else loop (n1 * acc) (base - 1)
  in
  loop 1 n2

let test () = print_int (ft_power 2 4)
let () = test ()
