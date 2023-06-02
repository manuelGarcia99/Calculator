(**Formula*)
(***M= 
  M(n-1) + S[0(i) para n - 2](M(i)*M(n-2-i))**)
(** The input is n*)
open Z

let savings = Array.make 10000000 (Z.of_int (0))
let my_savings_hash_table = Hashtbl.create 100000;;

let rec motzkin input =
  if(input=Z.of_int 0 || input= Z.of_int 1) then Z.of_int 1 
  else if (input= Z.of_int 2) then Z.of_int 2
  else if savings.(Z.to_int input) <> Z.of_int (0) then savings.(Z.to_int input) 
  else let output =
    motzkin (input- Z.of_int 1) + sum (Z.of_int 0) (input- Z.of_int 2) in
    let () = savings.(Z.to_int input) <- output in
    output

(**There may be an error*)
and sum arg1 arg2 =
  if (arg1 = arg2) then
     motzkin arg1 * motzkin (arg2  - arg1) 
  else
     if Hashtbl.mem my_savings_hash_table arg1 then 
      Hashtbl.find my_savings_hash_table arg1
     else 
      let output =
        motzkin arg1 * motzkin (arg2  - arg1) + sum (arg1 + Z.of_int 1) arg2 in 
        let () = Hashtbl.add my_savings_hash_table arg1 output in
        output;;
      

(**There may be an error*)


try
  let input_str = read_line () in
  let input = int_of_string input_str in
  let inputz = Z.of_int input in
  let resultado = Z.to_string (motzkin inputz) in
  Printf.printf"%s\n" resultado
with
  | exn -> print_endline "You need to use a positive integer or zero as input!"
  
