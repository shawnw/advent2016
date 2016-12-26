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
   
let rec pathfind_helper current_room pathsofar seed n paths =
  if current_room = end_room then
    paths := pathsofar :: !paths
  else if n < 50 then
    begin
      let hash = BatDigest.string (seed ^ pathsofar) |> BatDigest.to_hex in
      find_neighbors current_room hash
      |> BatList.iter (fun (neighbor, dir) ->
             let newpath = BatPrintf.sprintf "%s%c" pathsofar dir in
             pathfind_helper neighbor newpath seed (n + 1) paths)
    end

let pathfind seed =
    let paths = ref [] in
    pathfind_helper start_room "" seed 0 paths;
    let (paths, _) =
      BatList.map (fun path -> (path, BatString.length path)) !paths
      |> BatList.sort (fun (_,a) (_,b) -> compare a b)
      |> BatList.split in
    BatList.first paths
      
let _ = 
  let test1 = pathfind "ihgpwlah" in
  BatPrintf.printf "Test 1: %s\n" test1;
  let test2 = pathfind "kglvqrro" in
  BatPrintf.printf "Test 2: %s\n" test2;
  let test3 = pathfind "ulqzkmiv" in
  BatPrintf.printf "Test 3: %s\n" test3;
  let part1 = pathfind "pslxynzg" in
  BatPrintf.printf "Part 1: %s\n" part1
                   
  
                   
                   
