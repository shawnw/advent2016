open Batteries

type triangle = { a: int; b : int; c : int }

let make_triangle a' b' c' = { a = a'; b = b'; c = c' }
                  
let read_triangle () =
  Scanf.scanf " %d %d %d" make_triangle

let is_triangle tri =
  (tri.a + tri.b > tri.c) && (tri.a + tri.c > tri.b)
  && (tri.b + tri.c > tri.a)

let _ =
  let tris = ref 0 in
  try
    while true do
      let t = read_triangle () in
      if is_triangle t then
        incr tris
    done
  with
  | End_of_file -> print_int !tris; print_newline ()
                                                       
     
                            
