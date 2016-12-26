open Batteries

let dragon a =
  let buf = BatBuffer.create (BatString.length a * 2 + 1) in
  let b = BatString.map (function '0' -> '1' | '1' -> '0') a in
  BatString.rev_in_place b;
  BatBuffer.add_string buf a;
  BatBuffer.add_char buf '0';
  BatBuffer.add_string buf b;
  BatBuffer.contents buf

let rec checksum s  =
  let len = BatString.length s / 2 in
  let buf = BatBuffer.create len in
  for n = 0 to len - 1 do
    let a = s.[n * 2]
    and b = s.[(n * 2) + 1] in
    if a = b then
      BatBuffer.add_char buf '1'
    else
      BatBuffer.add_char buf '0'
  done;
    let chk = BatBuffer.contents buf in
    if String.length chk mod 2 = 1 then
      chk
    else
      checksum chk

let find_checksum seed len =
  let s = ref seed in
  while BatString.length !s < len do
    s := dragon !s
  done;
  let raw = BatString.sub !s 0 len in
  checksum raw

           
let _ =
  BatPrintf.printf "Test checksum: %s\n"
                   (find_checksum "10000" 20);
  BatPrintf.printf "Part 1 checksum: %s\n"
                   (find_checksum "00101000101111010" 272)
