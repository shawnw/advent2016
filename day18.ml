open Batteries

let is_trap row n =
  let len = BatString.length row in
  let left = if n = 0 then '.' else row.[n - 1]
  and center = row.[n]
  and right = if n = (len - 1) then '.' else row.[n + 1] in
  (left = '^' && center = '^' && right = '.')
  || (left = '.' && center = '^' && right = '^')
  || (left = '^' && center = '.' && right = '.')
  || (left = '.' && center = '.' && right = '^')
  
       
let next_row prev =
  let len = BatString.length prev in
  BatString.init len (fun n -> if is_trap prev n then '^' else '.')

let count_safe row =
  BatString.fold_left (fun sum ch -> if ch = '.' then sum + 1 else sum) 0 row
                 
let count_rows first rows =
  let row = ref first
  and safe = ref 0 in
  for n = 1 to rows do
    (*    print_endline !row; *)
    safe := !safe + count_safe !row;
    row := next_row !row
  done;
  !safe

let _ =
  let test1 = count_rows "..^^." 3 in
  BatPrintf.printf "Test 1: %d\n" test1;
  let test2 = count_rows ".^^.^.^^^^" 10 in
  BatPrintf.printf "Test 2: %d\n" test2;
  let start_row = read_line () in
  let part1 = count_rows start_row 40 in
  BatPrintf.printf "Part 1: %d\n" part1;
  let part2 = count_rows start_row 400_000 in
  BatPrintf.printf "Part 2: %d\n" part2
                   
                 
