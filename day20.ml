(* Requires 64 bit platform *)

open Batteries
       
let _ =
  let blacklist =
    BatIO.lines_of stdin 
    |> BatEnum.fold (fun iset line ->
           if BatString.contains line '-' then
             BatScanf.sscanf line "%d-%d" (fun lo hi -> BatISet.add_range lo hi iset)
           else
             BatISet.add (int_of_string line) iset) BatISet.empty in
  let max_num = 4294967295 in
  let whitelist = BatISet.compl blacklist
                  |> BatISet.remove_range BatInt.min_num (-1)
                  |> BatISet.remove_range (max_num + 1) BatInt.max_num in
  let lowest = BatISet.min_elt whitelist
  and count = BatISet.cardinal whitelist in
  BatPrintf.printf "Part 1: Lowest valid number: %d\n" lowest;
  BatPrintf.printf "Part 2: Total valid numbers: %d\n" count
