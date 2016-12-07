open Batteries

let _ =
  let cols = BatArray.init 8 (function _ -> BatHashtbl.create 26) in
  input_lines Pervasives.stdin |>
    BatEnum.iter
      (BatString.iteri (fun i ch ->
           BatHashtbl.modify_opt
             ch
             (function Some i -> Some (i + 1) | None -> Some 1)
             cols.(i)));
  BatArray.iter (fun tbl ->
      let ch, v = 
        BatHashtbl.fold (fun ch v mx ->
            if v > snd mx then (ch, v) else mx) tbl ('_', -1) in
        print_char ch) cols;
  print_newline ()
                
                                 
