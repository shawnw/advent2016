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
      let ch, v = BatHashtbl.enum tbl |>
                    BatEnum.reduce (fun (ch1, v1) (ch2, v2) ->
                        if v1 > v2 then
                          (ch1, v1)
                        else
                          (ch2, v2)) in
      print_char ch) cols;
  print_newline ()
                                 
