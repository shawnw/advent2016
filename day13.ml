open Batteries

type location = Open | Wall

let magic = ref 10 (* 1358 *)
              
let location_type x y =
  let v = x*x + 3*x + 2*x*y + y + y*y + !magic in
  let bits = BatInt.popcount v in
  if bits mod 2 == 0 then
    Open
  else
    Wall

type point = { x : int; y : int }

let taxi_distance p1 p2 =
  BatInt.abs (p1.x - p2.x) + BatInt.abs (p1.y - p2.y)

let find_score tbl p =
  BatHashtbl.find_default tbl p BatInt.max_num

let min_score candidates distances =
  let p, _ =
    BatHashtbl.fold
      (fun p v (pmin, dmin) ->
        let score = find_score distances p in
        if score < dmin then
          (p, score)
        else
          (pmin, dmin)) candidates ({x = -1; y = -1}, BatInt.max_num) in
  p

let get_neighbors p =
  let neighbors = ref [] in
  if p.x > 0 then
    neighbors := { p with x = p.x -  1 } :: !neighbors;
  neighbors := { p with x = p.x + 1 } :: !neighbors;
  if p.y > 0 then
    neighbors := { p with y = p.y - 1 } :: !neighbors;
  neighbors := { p with y = p.y + 1 } :: !neighbors;
  BatList.filter (fun np -> match location_type np.x np.y with
                            | Open -> true
                            | Wall -> false) !neighbors
                                           
exception Found of point
    
let astar start goal =
  let visited = BatHashtbl.create 500
  and candidates = BatHashtbl.create 500
  and guess_distances = BatHashtbl.create 500
  and calculated_distances = BatHashtbl.create 500 in
  BatHashtbl.add candidates start true;
  BatHashtbl.add calculated_distances start 0;
  BatHashtbl.add guess_distances start (taxi_distance start goal);
  try
    while not @@ BatHashtbl.is_empty candidates do
      let current = min_score candidates guess_distances in
      if current = goal then
        raise (Found current);
      BatHashtbl.remove candidates current;
      BatHashtbl.add visited current true;
      let neighbors = get_neighbors current                                    
      and tentative_score = (find_score calculated_distances current) + 1 in
      BatList.iter (fun p ->
          if not @@ BatHashtbl.mem visited p then
            begin
              if not @@ BatHashtbl.mem candidates p then
                BatHashtbl.add candidates p true;
              if tentative_score < find_score calculated_distances p then
                begin
                  BatHashtbl.add calculated_distances p tentative_score;
                  BatHashtbl.add guess_distances p (tentative_score + (taxi_distance p goal))                  
                end              
            end) neighbors;
    done;
    -1
  with Found p -> BatHashtbl.find calculated_distances p
                                                            

let rec expand n tbl prev =
  if n = 0 then
    BatHashtbl.length tbl
  else
    begin
      let neighbors = BatList.map get_neighbors prev
                      |> BatList.concat
                      |> BatList.filter (fun p -> not @@ BatHashtbl.mem tbl p) in
      BatList.iter (fun p -> BatHashtbl.replace tbl p true) neighbors;
      expand (n - 1) tbl neighbors
    end
            

let _ =
  let starting_point = { x = 1; y = 1 } in
  let test_len = astar starting_point { x = 7; y = 4 } in
  BatPrintf.printf "Test: Shortest path length is %d\n" test_len;
  magic := 1358;
  let part1_len = astar starting_point { x = 31; y = 39 } in
  BatPrintf.printf "Part 1: Shortest path length is %d\n" part1_len;
  let part2_table = BatHashtbl.create 1000 in
  BatHashtbl.add part2_table starting_point true;
  let part2_count = expand 50 part2_table [ starting_point ] in
  BatPrintf.printf "Part 2: Rooms reachable in 50 steps: %d\n" part2_count
                  

                        
