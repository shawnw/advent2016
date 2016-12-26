module type LocationType = sig
  type t
  val guess_distance: t -> t -> int
  val neighbors: t -> (t * int) list
  val equal: t -> t -> bool
  val hash: t -> int
end

module Make(Location:LocationType) : sig
  val distance: Location.t -> Location.t -> int
end = struct

  exception Found of Location.t  

  module Hash = BatHashtbl.Make(Location)
                       
  let find_score tbl p =
    Hash.find_default tbl p BatInt.max_num

  let min_score candidates distances =
    match
      Hash.fold
        (fun p v (pmin, dmin) ->
          let score = find_score distances p in
          if score < dmin then
            (Some p, score)
          else
            (pmin, dmin)) candidates (None, BatInt.max_num)
    with
    | Some p, _ -> p
    | None, _ -> raise Not_found
                                           
  let distance start goal =
    let visited = Hash.create 500
    and candidates = Hash.create 500
    and guess_distances = Hash.create 500
    and calculated_distances = Hash.create 500 in
    Hash.add candidates start true;
    Hash.add calculated_distances start 0;
    Hash.add guess_distances start (Location.guess_distance start goal);
  try
    while not @@ Hash.is_empty candidates do
      let current = min_score candidates guess_distances in
      if current = goal then
        raise (Found current);
      Hash.remove candidates current;
      Hash.add visited current true;
      let neighbors = Location.neighbors current                                    
      and tentative_score = find_score calculated_distances current in
      BatList.iter (fun (p, dist) ->
          let tentative_score = tentative_score + dist in
          if not @@ Hash.mem visited p then
            begin
              if not @@ Hash.mem candidates p then
                Hash.add candidates p true;
              if tentative_score < find_score calculated_distances p then
                begin
                  Hash.add calculated_distances p tentative_score;
                  Hash.add guess_distances p (tentative_score + (Location.guess_distance p goal))                  
                end              
            end) neighbors;
    done;
    -1
  with Found p -> Hash.find calculated_distances p
                                                            
end
