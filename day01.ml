open Batteries

type pair = { x : int; y : int }
type direction = North | East | West | South
type rotate = Left | Right

let new_direction d r = 
  match d with
  | North when r = Left -> West
  | North when r = Right -> East
  | East when r = Left -> North
  | East when r = Right -> South
  | South when r = Left -> East
  | South when r = Right -> West
  | West when r = Left -> South
  | West when r = Right -> North

let to_direction = function
  | "L" -> Left
  | "R" -> Right
  | _ -> raise (Invalid_argument "Direction must be L or R")


let adjust dir dist pos =
  match dir with
  | North -> { pos with y = pos.y + dist }
  | East -> { pos with x = pos.x + dist }
  | South -> { pos with y = pos.y - dist }
  | West -> { pos with x = pos.x - dist }


let re = Str.regexp " *\\([LR]\\)\\([0-9]+\\) *"

let move dir (d, pos) = 
  if Str.string_match re dir 0 then begin
    let r = Str.matched_group 1 dir 
    and dist = int_of_string (Str.matched_group 2 dir) in
    let newdir = new_direction d (to_direction r) in
    (newdir, adjust newdir dist pos)
  end else
    raise (Invalid_argument ("Invalid direction string: " ^ dir))

let taxi orig dest =
  let a1 = abs (orig.x - dest.x)
  and a2 = abs (orig.y - dest.y) in
  a1 + a2


let main () = 
  let startc = {x = 0; y = 0 }
  and currc = ref (North, { x = 0; y = 0 })
  and dirs = BatString.nsplit (input_all Pervasives.stdin) "," in
  List.iter (fun d -> currc := move d !currc) dirs;
  Printf.printf "Ending coordinates: x = %d, y = %d\n" (snd !currc).x (snd !currc).y;
  Printf.printf "Distance: %d\n" (taxi startc (snd !currc))


let _ = main ()
      
