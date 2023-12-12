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

let cache = Hashtbl.create 1_000_000

let rec loop cl il spring =
  match Hashtbl.find_opt cache ( cl, il, spring ) with
  | Some x -> x
  | None ->
    let res =
    match cl, il, spring with
    | [],         [],       _ ->     1
    | [],         [0],      _ ->     1
    | [],         _ :: _,   _ ->     0 (* some data left *)
    | '#' :: _,   [],       _ ->     0 (* spring left but count 0 *)
    | _ :: clt,   [],       _ ->     if List.mem '#' clt then 0 else 1 (* spring somewhere in rest but count 0 *)
      (* spring *)
    | '#' :: _,   0 :: _,   true ->  0 (* spring group already fully allocated *)
    | '#' :: clt, g :: ilt, _ ->     loop clt ((g - 1) :: ilt) true
      (* space *)
    | '.' :: clt, 0 :: ilt, _ ->     loop clt ilt false
    | '.' :: clt, g :: ilt, false -> loop clt (g :: ilt) false
    | '.' :: _,   _ :: _,   true ->  0 (* more spring to allocated - failed *)
      (* run of springs *)
    | '?' :: clt, 0 :: ilt, true ->  loop clt ilt false
    | '?' :: clt, g :: ilt, true ->  loop clt ((g - 1) :: ilt) true
      (* could be a spring or not *)
    | '?' :: clt, g :: ilt, _ ->    (loop clt (g :: ilt) false) + (loop clt ((g - 1) :: ilt) true)
    | _ -> assert false in
    let () = Hashtbl.add cache ( cl, il, spring ) res in
    res

let sum = List.map2 (fun row group -> loop row group false) rows groups
         |> List.fold_left ( + ) 0

let () = Printf.printf "part 1 %i\n" sum


let rows = List.map (fun row -> row @ ['?'] @ row @ ['?'] @ row @ ['?'] @ row @ ['?'] @ row) rows
let groups = List.map (fun group -> group @ group @ group @ group @ group) groups

let sum = List.map2 (fun row group ->
    loop row group false) rows groups
         |> List.fold_left ( + ) 0

let () = Printf.printf "part 2 %i\n" sum
