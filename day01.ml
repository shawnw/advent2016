open Batteries

type pair = { x : int; y : int }
type direction = North | East | West | South
type rotate = Left | Right

module OrderedPair = struct
  type t = pair
  let compare = Pervasives.compare 
end

module Cache = BatSet.Make(OrderedPair)

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

let rec add_points cache orig dest =
  if orig.x = dest.x && orig.y = dest.y then
    (false, orig, cache)
  else begin
    let newpoint = 
    if orig.x < dest.x then
      { orig with x = orig.x + 1 }
    else if orig.x > dest.x then
      { orig with x = orig.x - 1 }
    else if orig.y < dest.y then
      { orig with y = orig.y + 1 }
    else if orig.y > dest.y then
      { orig with y = orig.y - 1 } 
    else
      orig in
    if Cache.mem newpoint cache then
      (true, newpoint, cache)
    else begin
      (* Printf.printf "Adding {%d,%d} to cache.\n" newpoint.x newpoint.y; *)
      add_points (Cache.add newpoint cache) newpoint dest
    end
  end

let first_repeat dirs loc =
  let points = Cache.add (snd loc) Cache.empty in
  let rec helper loc cache = function
    | hd :: tl ->
      let (newdir, newloc) = move hd loc in
      let (found, repeat, cache) = add_points cache (snd loc) newloc in
      if found then
	repeat
      else
	helper (newdir, newloc) cache tl
    | [] -> raise (Invalid_argument "No repeated coordinates!") in
  helper loc points dirs


let main () = 
  let startc = {x = 0; y = 0 }
  and currc = ref (North, { x = 0; y = 0 })
  and dirs = BatString.nsplit (input_all Pervasives.stdin) "," in
  List.iter (fun d -> currc := move d !currc) dirs;
  Printf.printf "Ending coordinates: x = %d, y = %d\n" (snd !currc).x (snd !currc).y;
  Printf.printf "Distance: %d\n" (taxi startc (snd !currc));
  let repeat = first_repeat dirs (North, { x = 0; y = 0}) in
  Printf.printf "First repeated coordinate: x = %d, y = %d\n" repeat.x repeat.y;
  Printf.printf "Distance: %d\n" (taxi startc repeat)


let _ = main ()
      
