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
  y : int;
  x : int;
}

module Contraption = Map.Make (struct
  type t = coord
  let compare = compare
end)

let contraption, _ = List.fold_left (fun (contraption, location) line ->
    String.fold_left (fun (contraption, location) ch ->
      (Contraption.add location ch contraption), { x = location.x + 1; y = location.y }
    ) (contraption, { x = 1; y = location.y + 1 }) line
  ) (Contraption.empty, { x = 1; y = 0 }) (List.rev read_input)

let print c b =
  let _ = Contraption.fold (fun k ch y ->
    let () = if k.y != y then Printf.printf "\n" in
    let () = match List.assoc_opt k b with
    | None -> Printf.printf "%c" ch
    | Some _ -> Printf.printf "*" in
    k.y) c 1 in
  Printf.printf "\n\n"

let () = print contraption []

let beams = [{ x = 0; y = 1 }, { x = 1; y = 0 }]

let rec loop lst = function
  | [] -> lst
  | (pos, dir) :: tl ->
    let new_pos = { x = pos.x + dir.x; y = pos.y + dir.y } in
    match Contraption.find_opt new_pos contraption with
    | None -> loop lst tl
    | Some '|' ->
      if dir.y = 0
      then loop (lst @ [new_pos, { x = 0; y = 1}; new_pos, { x = 0; y = -1 }]) tl
      else loop (lst @ [new_pos, dir]) tl
    | Some '-' ->
      if dir.x = 0
      then loop (lst @ [new_pos, { x = 1; y = 0}; new_pos, { x = -1; y = 0 }]) tl
      else loop (lst @ [new_pos, dir]) tl
    | Some '.' -> loop (lst @ [new_pos, dir]) tl
    | Some '\\' -> loop (lst @ [new_pos, { x = dir.y ; y = dir.x }]) tl
    | Some '/' -> loop (lst @ [new_pos, { x = - dir.y ; y = - dir.x }]) tl
    | Some _ -> assert false

let cache = Hashtbl.create 10_000

let rec run beams energized =
  let beams = loop [] beams in
  let energized = List.fold_left (fun e (pos, _) -> Contraption.add pos true e) energized beams in
  let beams = List.filter (fun (pos, dir) -> not (Hashtbl.mem cache ( pos, dir ))) beams in
  let () = List.iter (fun (pos, dir) -> Hashtbl.add cache ( pos, dir ) true) beams in
(*  let () = print contraption beams in *)
  let ne = Contraption.cardinal energized in
  if List.length beams > 0
  then run beams energized
  else ne

let part1 = run beams Contraption.empty
let () = Printf.printf "part 1: %i\n" part1


let possible_starts =
  let min, _ = Contraption.min_binding contraption in
  let max, _ = Contraption.max_binding contraption in
  let rec loop_vertical y lst =
    if y >= min.y
    then loop_vertical (y - 1) ([( { x = 0; y }, { x = 1; y = 0 } ); ( { x = max.x + 1; y }, { x = -1; y = 0 } )] @ lst)
    else lst in
  let rec loop_horizontal x lst =
    if x >= min.x
    then loop_horizontal (x - 1) ([( { x; y = 0 }, { x = 0; y = 1 } ); ( { x; y = max.y + 1 }, { x = 0; y = -1 } )] @ lst)
    else lst in
  loop_vertical (max.y) [] @ loop_horizontal (max.y) []


let results = List.map (fun (pos, dir) ->
    let () = Hashtbl.reset cache in
    let sum = run [(pos, dir)] Contraption.empty in
    sum
  ) possible_starts

let part2 = List.fold_left (fun m v -> max m v) 0 results
let () = Printf.printf "part 2: %i\n" part2
