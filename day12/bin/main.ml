let read_input =
  let ic = open_in "input" in
  let rec loop input lines =
    try
      let line = input_line input in
      loop input (line :: lines)
    with End_of_file ->
      close_in input;
      lines
  in
  loop ic []

let rows, groups = List.fold_left (fun (rows, groups) line ->
    let split = String.split_on_char ' ' line in
    let row = List.nth split 0 in
    let row = List.init (String.length row) (String.get row) in
    let cgroups = List.nth split 1 in
    let cgroups = String.split_on_char ',' cgroups in
    let cgroups = List.map (int_of_string) cgroups in
    row :: rows, cgroups :: groups
  ) ([], []) read_input

let rec loop r cg spring =
  (*
  let () = Printf.printf "%c %i %s\n" (if List.length r > 0 then List.hd r else '-') (if List.length cg > 0 then List.hd cg else -1) (if spring then "true" else "false") in
     *)
  match r, cg, spring with
  | [],         [],       _ -> 1
  | [],         [0],      _ -> 1
  | [],         _ :: _,   _ -> 0 (* some data left *)
  | '#' :: _,   [],       _ -> 0 (* spring left but count 0 *)
  | _ :: rtl,   [],       _ -> if List.mem '#' rtl then 0 else 1 (* spring somewhere in rest but count 0 *)
    (* spring *)
  | '#' :: _,   0 :: _,   true -> 0 (* spring group already fully allocated *)
  | '#' :: rtl, g :: gtl, _ -> loop rtl ((g - 1) :: gtl) true
    (* space *)
  | '.' :: rtl, 0 :: gtl, _ -> loop rtl gtl false
  | '.' :: rtl, g :: gtl, false -> loop rtl (g :: gtl) false
  | '.' :: rtl, g :: gtl, true -> 0 (* more spring to allocated - failed *)
    (* run of springs *)
  | '?' :: rtl, 0 :: gtl, true -> loop rtl gtl false
  | '?' :: rtl, g :: gtl, true -> loop rtl ((g - 1) :: gtl) true
    (* could be a spring or not *)
  | '?' :: rtl, g :: gtl, _ -> (loop rtl (g :: gtl) false) + (loop rtl ((g - 1) :: gtl) true)
  | _ -> assert false

let () = List.iter2 (fun row group ->
    let () = List.iter (Printf.printf "%c") row in
    let () = List.iter (Printf.printf ",%i") group in
    Printf.printf " = %i\n" (loop row group false)
  ) rows groups

let sum = List.map2 (fun row group -> loop row group false) rows groups
         |> List.fold_left ( + ) 0

let () = Printf.printf "part 1 %i" sum
