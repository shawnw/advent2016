open Batteries

type direction = L | R | U | D

let move = function
  | 1, U -> 1
  | 1, R -> 2
  | 1, D -> 4
  | 1, L -> 1
  | 2, U -> 2
  | 2, R -> 3
  | 2, D -> 5
  | 2, L -> 1
  | 3, U -> 3
  | 3, R -> 3
  | 3, D -> 6
  | 3, L -> 2
  | 4, U -> 1
  | 4, R -> 5
  | 4, D -> 7
  | 4, L -> 4
  | 5, U -> 2
  | 5, R -> 6
  | 5, D -> 8
  | 5, L -> 4
  | 6, U -> 3
  | 6, R -> 6
  | 6, D -> 9
  | 6, L -> 5
  | 7, U -> 4
  | 7, R -> 8
  | 7, D -> 7
  | 7, L -> 7
  | 8, U -> 5
  | 8, R -> 9
  | 8, D -> 8
  | 8, L -> 7
  | 9, U -> 6
  | 9, R -> 9
  | 9, D -> 9
  | 9, L -> 8
  | _, _ -> raise (Invalid_argument "Number out of range")

let c2d = function
  | 'L' -> L
  | 'R' -> R
  | 'U' -> U
  | 'D' -> D
  | c -> Printf.printf "Unknown character '%c'\n" c; raise (Invalid_argument "No such direction")

let _ =
  try
    let button = ref 5 in
    while true do
      let line = read_line () in
      let dirs = BatList.map c2d (BatString.to_list line) in
      button := BatList.fold_left (fun key dir -> move (key, dir)) !button dirs;
      print_int !button
    done
  with End_of_file -> print_newline ()
                                    
            
