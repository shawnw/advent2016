open Batteries

let hashes = BatHashtbl.create 35_000
                               
exception Found of char
                               
let match3 s =
  try
    for n = 0 to (BatString.length s) - 3 do
      if s.[n] = s.[n + 1] && s.[n] = s.[n + 2] then
        raise (Found s.[n])
    done;
    None
  with Found ch -> Some ch
                  
let match5 s ch =
  let test = BatString.make 5 ch in
  BatString.exists s test

let calc_hash plain stretched =
  match BatHashtbl.find_option hashes plain with
  | Some hash -> hash
  | None ->
     let hash = ref (BatDigest.string plain |> BatDigest.to_hex) in
     if stretched then
       for n = 1 to 2016 do
         hash := BatDigest.string !hash |> BatDigest.to_hex         
       done;
     BatHashtbl.add hashes plain !hash;
     !hash
                        
let is_key salt n stretched =
  let plain = BatPrintf.sprintf "%s%d" salt n in
  let hash = calc_hash plain stretched in
  match match3 hash with
  | Some ch ->
     begin
       let found = ref false
       and step = ref 0
       and counter = ref (n + 1) in
       while !found <> true && !step < 1000 do
         let plain = BatPrintf.sprintf "%s%d" salt !counter in
         let hash = calc_hash plain stretched in
         if match5 hash ch then
             found := true;
         incr step;
         incr counter
       done;
       !found      
     end
| None -> false


let find_64th_key salt stretched =
  let counter = ref 0 in
  BatHashtbl.clear hashes;
  for i = 1 to 64 do
    while not @@ is_key salt !counter stretched do
      incr counter
    done;
    incr counter;
  done;
  !counter - 1

let _ =
  let test = find_64th_key "abc" false in
  BatPrintf.printf "Test 1: Index %d\n" test;
  let test2 = find_64th_key "abc" true in
  BatPrintf.printf "Test 2: Index %d\n" test2;
  let part1 = find_64th_key "jlmsuwbz" false in
  BatPrintf.printf "Part 1: Index %d\n" part1;
  let part2 = find_64th_key "jlmsuwbz" true in
  BatPrintf.printf "Part 2: Index %d\n" part2
    
