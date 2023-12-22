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
      [] in
    brick :: bricks
  ) [] read_input

let () = List.iter (fun brick ->
    let () = List.iter (fun pos -> Printf.printf "(%i,%i,%i)," pos.x pos.y pos.z) brick in
    Printf.printf "\n"
  ) bricks

module Island = Map.Make (struct
  type t = coord
  let compare = compare
end)

let n, island = List.fold_left (fun (i, island) brick ->
    (i + 1, List.fold_left (fun island pos -> Island.add pos i island) island brick)
  ) (1, Island.empty) bricks

let bricks = List.init (n - 1) (fun i -> i + 1)

let () = Printf.printf "%i bricks\n" n

let print island =
  for z = 10 downto 1 do
    for y = 0 to 2 do
      for x = 0 to 2 do
        match Island.find_opt { x; y; z } island with
        | None -> Printf.printf "."
        | Some x -> Printf.printf "%i" x
      done ;
      Printf.printf "          " ;
    done ;
    Printf.printf "\n" ;
  done ;
    Printf.printf "\n"

let () = print island

let drop n island =
  let brick = Island.filter (fun _ v -> v = n) island in
  let can_fall = Island.fold (fun pos _ hit ->
      hit && pos.z > 1 &&
      match Island.find_opt { x = pos.x; y = pos.y; z = pos.z - 1 } island with
      | None -> true
      | Some i -> i = n
    ) brick true in
  can_fall, 
  (if can_fall then
      Island.fold (fun pos _ i -> Island.remove pos i) brick island |>
      Island.fold (fun pos x i -> Island.add { x = pos.x; y = pos.y; z = pos.z - 1 } x i) brick
  else island)

let rec drop_all island num =
  let movement, island = List.fold_left (fun (movement, island) n ->
      let cf, island = drop n island in
      (cf || movement), island) (false, island) bricks in
    let () = Printf.printf "dropping\n" in
    let () = flush stdout in
  if movement then drop_all island num else island

let island = drop_all island n

let part1 = List.fold_left (fun sum n ->
    let () = Printf.printf "considering %i\n" n in
    let () = flush stdout in
  let one_removed = Island.filter (fun _ v -> v <> n) island in
  let did_move, _ = List.fold_left (fun (movement, island) m -> 
      let cf, island = drop m island in
      (cf || movement), island) (false, one_removed) (List.filter (fun m -> n <> m) bricks) in
  let () = Printf.printf "%i removed - %s\n" n (if did_move then "movement" else "solid") in
    if did_move then sum else sum + 1
) 0 bricks

let () = Printf.printf "Part 1 - %i\n" part1
