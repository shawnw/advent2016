(* % ./day03.byte [2] < day03.txt *)

open Batteries

type triangle = { a: int; b : int; c : int }

let make_triangle a' b' c' = { a = a'; b = b'; c = c' }

let triangle_of_string str =
  Scanf.sscanf str " %d %d %d" make_triangle
                               
let read_triangle_by_row () =
  Scanf.scanf " %d %d %d" make_triangle

let read_triangles_by_col () =
  let make_triangles a1 a2 a3 b1 b2 b3 c1 c2 c3 =
    [ make_triangle a1 b1 c1; make_triangle a2 b2 c2;
      make_triangle a3 b3 c3 ] in
  Scanf.scanf " %d %d %d %d %d %d %d %d %d" make_triangles
  
let is_triangle tri =
  (tri.a + tri.b > tri.c) && (tri.a + tri.c > tri.b)
  && (tri.b + tri.c > tri.a)

let part1 () =
  let tris = BatEnum.fold (fun sum line ->
                 if triangle_of_string line |> is_triangle then
                   sum + 1
                 else
                   sum) 0 (input_lines Pervasives.stdin) in
  Printf.printf "Part 1: %d\n" tris

let part2 () =
  let tris = ref 0 in
  try
    while true do
      let ts = read_triangles_by_col () in
      tris := !tris + (List.filter is_triangle ts |> List.length)
    done
  with
  | End_of_file -> print_string "Part 2: ";
                   print_int !tris;
                   print_newline ()
                                 
let _ =
  if Array.length Sys.argv = 1 then
    part1 ()
  else
    part2 ()
          
                            
