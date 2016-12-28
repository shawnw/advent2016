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
  let max_number = 4294967295 in
  let lowest = ref 0 in
  while BatISet.mem !lowest blacklist do
    incr lowest
  done;
  BatPrintf.printf "Part 1: Lowest valid number: %d\n" !lowest;
  let blacklist_size = BatISet.cardinal blacklist in
  BatPrintf.printf "Part 2: Total valid numbers: %d\n" (max_number + 1 - blacklist_size)
