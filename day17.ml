open Batteries

type point = { x : int; y : int }

let start_room = { x = 0; y = 0 }
let end_room = { x = 3; y = 3 }

let is_open = function
  | 'b' | 'c' | 'd' | 'e' | 'f' -> true
  | _ -> false
                 
exception Found of string

let find_neighbors room hash =
  let neighbors = ref [] in
  if is_open hash.[0] && room.x > 0 then
    neighbors := ({ room with x = room.x - 1 }, 'U') :: !neighbors;
  if is_open hash.[1] && room.x < 3 then
    neighbors := ({ room with x = room.x + 1 }, 'D') :: !neighbors;
  if is_open hash.[2] && room.y > 0 then
    neighbors := ({ room with y = room.y - 1 }, 'L') :: !neighbors;
  if is_open hash.[3] && room.y < 3 then
    neighbors := ({room with y = room.y + 1 }, 'R') :: !neighbors;
  !neighbors                                                        

module RoomAStar =
  Astar.Make(struct
              type t = point * string * string

              let guess_distance (a, _, _) (b, _, _) =
                BatInt.abs (a.x - b.x) + BatInt.abs (a.y - b.y)
              
                                 
              let neighbors (room, pathsofar, seed) =
                let hash =
                  BatDigest.string (seed ^ pathsofar) |> BatDigest.to_hex in
                find_neighbors room hash
                |> BatList.map (fun (r,d) ->
                       ((r, (BatPrintf.sprintf "%s%c" pathsofar d), seed), 1))

              let compare (a, _, _) (b, _, _) = compare a b
              let equal = (=)
              let hash = BatHashtbl.hash 
            end)

let pathfind seed =
  let p = RoomAStar.path (start_room, "", seed) (end_room, "", seed) in
  let (_, path, _) = BatList.last p in
  path
               
let _ = 
  let test1 = pathfind "ihgpwlah" in
  BatPrintf.printf "Test 1: %s\n" test1;
  let test2 = pathfind "kglvqrro" in
  BatPrintf.printf "Test 2: %s\n" test2;
  let test3 = pathfind "ulqzkmiv" in
  BatPrintf.printf "Test 3: %s\n" test3;
  let part1 = pathfind "pslxynzg" in
  BatPrintf.printf "Part 1: %s\n" part1
                   
  
                   
                   
