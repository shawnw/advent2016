(* % ./day05.native INPUT *)

open Batteries

let is_magic md5 =
  BatChar.code md5.[0] lor BatChar.code md5.[1] lor
    (BatChar.code md5.[2] land 0xF0) = 0

let find_password seed =
  let password1 = BatString.create 8
  and password2 = BatString.make 8 '_'
  and idx1 = ref 0
  and idx2 = ref 0
  and i = ref 0 in
  let make n = seed ^ (string_of_int n) in
  while !idx1 < 8 || !idx2 < 8 do
    let md5 = ref (BatDigest.string (make !i)) in
    while not (is_magic !md5) do
      incr i;
      md5 := BatDigest.string (make !i)
    done;
    incr i;
    let md5hex = BatDigest.to_hex !md5 in
    if !idx1 < 8 then begin
        BatString.set password1 !idx1 md5hex.[5];
        incr idx1
      end;
    if BatChar.is_digit md5hex.[5] then begin
        let n = BatChar.code md5hex.[5] - BatChar.code '0' in
        if n < 8 && password2.[n] = '_' then begin
            BatString.set password2 n md5hex.[6];
            incr idx2
          end
      end              
  done;
  password1, password2

let finder seed =
  let pass1, pass2 = find_password seed in
  Printf.printf "For %s:\n" seed;
  Printf.printf "1: %s\n" pass1;
  Printf.printf "2: %s\n" pass2

let _ =
  finder Sys.argv.(1)

                

 

              

              
