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

type coord = {
  z : int;
  y : int;
  x : int;
}

let bricks = List.fold_left (fun bricks line ->
    let csv_to_coord s =
      let values = String.split_on_char ',' s in
      let values = List.map (int_of_string) values in
      { x = List.nth values 0; y = List.nth values 1; z = List.nth values 2 } in
    let coords = String.split_on_char '~' line in
    let head = csv_to_coord (List.nth coords 0) in
    let tail = csv_to_coord (List.nth coords 1) in
    let brick = if head.x <> tail.x then List.init (1 + tail.x - head.x) (fun i -> { x = head.x + i; y = head.y; z = head.z }) else
      if head.y <> tail.y then List.init (1 + tail.y - head.y) (fun i -> { x = head.x; y = head.y + i; z = head.z }) else
      if head.z <> tail.z then List.init (1 + tail.z - head.z) (fun i -> { x = head.x; y = head.y; z = head.z + i }) else
      [ { x = head.x; y = head.y; z = head.z } ] in
    brick :: bricks
  ) [] read_input

let () = List.iter (fun brick ->
    let () = List.iter (fun pos -> Printf.printf "(%i,%i,%i)," pos.x pos.y pos.z) brick in
    Printf.printf "\n"
  ) bricks

let island = Hashtbl.create 1000

let n = List.fold_left (fun i brick ->
    let () = List.iter (fun pos -> Hashtbl.add island pos i) brick in
    i + 1
  ) 0 bricks

let bricks = List.init n (fun i -> i)

let () = Printf.printf "%i bricks\n" n

let print island =
  for z = 300 downto 1 do
    for y = 0 to 9 do
      for x = 0 to 9 do
        match Hashtbl.find_opt island { x; y; z } with
        | None -> Printf.printf "."
        | Some x -> Printf.printf "%i" (x mod 10)
      done ;
      Printf.printf " " ;
    done ;
    Printf.printf "\n" ;
  done ;
    Printf.printf "\n"

let () = print island

let drop island n =
  let brick = Hashtbl.fold (fun pos v lst -> if v = n then pos :: lst else lst) island [] in
  let () = assert (List.length brick <> 0) in
  let can_fall = List.fold_left (fun hit pos ->
      hit && pos.z > 1 &&
      match Hashtbl.find_opt island { x = pos.x; y = pos.y; z = pos.z - 1 } with
      | None -> true
      | Some i -> i = n
    ) true brick in
  let () = if can_fall then
      let () = List.iter (Hashtbl.remove island) brick in
      List.iter (fun pos -> Hashtbl.add island { x = pos.x; y = pos.y; z = pos.z - 1 } n) brick in
  can_fall

let rec drop_all () =
  let moved = List.fold_left (fun moved n -> if drop island n then moved + 1 else moved) 0 bricks in
  let () = Printf.printf "dropping\n" in
  let () = print island in
  let () = flush stdout in
  if moved > 0 then drop_all ()

let () = drop_all ()

let can_drop island n =
  let brick = Hashtbl.fold (fun pos v lst -> if v = n then pos :: lst else lst) island [] in
  let () = assert (List.length brick <> 0) in
  let can_fall = List.fold_left (fun hit pos ->
      hit && pos.z > 1 &&
      match Hashtbl.find_opt island { x = pos.x; y = pos.y; z = pos.z - 1 } with
      | None -> true
      | Some i -> i = n
    ) true brick in
  can_fall 

let part1 = List.fold_left (fun sum n ->
  let copy = Hashtbl.copy island in
  let brick = Hashtbl.fold (fun pos v lst -> if v = n then pos :: lst else lst) copy [] in
  let () = List.iter (Hashtbl.remove copy) brick in
  let number_moved = List.fold_left (fun movement m ->
    if can_drop copy m then movement + 1 else movement) 0 (List.filter (fun m -> n <> m) bricks) in
  let () = Printf.printf "brick %i removed - %i moved\n" n number_moved in
  let () = flush stdout in
    if number_moved > 0 then sum else sum + 1
) 0 bricks

let () = Printf.printf "Part 1 - %i\n" part1
let () = flush stdout


let part2 = List.fold_left (fun sum n ->
  let copy = Hashtbl.copy island in
  let brick = Hashtbl.fold (fun pos v lst -> if v = n then pos :: lst else lst) copy [] in
  let () = List.iter (Hashtbl.remove copy) brick in
  let brick_list = List.filter (fun m -> n <> m) bricks in
  let rec loop lst = function
    | true -> lst
    | false -> let moved = List.fold_left (fun lst m ->
        if drop copy m then m :: lst else lst) [] brick_list in
           loop (moved @ lst) (List.length moved = 0) in
  let all_moved = loop [] false in
  let () = Printf.printf "brick %i removed - %i moved : " n (List.length all_moved) in
  let () = List.iter (Printf.printf "%i,") all_moved in
  let () = Printf.printf "\n" in
  let () = flush stdout in
  sum + (List.sort_uniq (compare) all_moved |> List.length)
) 0 bricks

let () = Printf.printf "Part 2 - %i\n" part2
