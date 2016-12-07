open Batteries

let checksum s =
  let opbr = BatString.index s '[' in
  BatString.sub s (opbr + 1) 5

let designator s =
  BatString.rchop ~n:7 s

let room_name s =
  let n = BatString.rindex s '-' in
  BatString.rchop ~n s
  
let sector_id s =
  let opbr = BatString.index s '[' in
  let lastdash = BatString.rindex_from s opbr '-' in
  let id = BatString.sub s (lastdash + 1) (opbr - lastdash - 1) in
  int_of_string id
                  
let calc_checksum freqs =
  let cmp a b =
    match compare (snd b) (snd a) with
    | 0 -> compare (fst a) (fst b)
    | x -> x in
  let byfreq = BatHashtbl.enum freqs |> BatList.of_enum |> BatList.sort cmp in
  let first = BatList.take 5 byfreq |> BatList.map fst in
  BatString.of_list first
                                                                   
let is_real room =
  let name = designator room
  and chksum = checksum room
  and freqs = BatHashtbl.create 26 in
  let update_freq ch =
    if BatChar.is_letter ch then
      BatHashtbl.modify_opt ch
                            (function Some n -> Some (n + 1) | None -> Some 1) freqs in
  BatString.iter update_freq name;
  let found = calc_checksum freqs in
  found = chksum

let decrypt_name enc rot =
  let letters = "abcdefghijklmnopqrstuvwxyz" in
  let rotate = function
    | '-' -> ' '
    | ch -> letters.[((BatString.index letters ch) + rot) mod 26] in
  BatString.map rotate enc

let _ =
  let sumsecs = ref 0 in
  let storagesec = ref (-1) in
  try
    while true do
      let room = read_line () in
      if is_real room then begin
          let id = sector_id room in
          let decrypted = decrypt_name (room_name room) id in
          if BatString.exists decrypted "northpole" then
            storagesec := id;
          sumsecs := !sumsecs + id;
        end
    done
  with
  | End_of_file ->
     Printf.printf "Part 1: %d\nPart 2: %d\n" !sumsecs !storagesec
                   
      
                  
