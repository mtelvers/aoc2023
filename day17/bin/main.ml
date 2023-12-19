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
      (Contraption.add location (int_of_string (String.make 1 ch)) contraption), { x = location.x + 1; y = location.y }
    ) (contraption, { x = 1; y = location.y + 1 }) line
  ) (Contraption.empty, { x = 1; y = 0 }) (List.rev read_input)

let print c r =
  let _ = Contraption.fold (fun k v y ->
    let () = if k.y != y then Printf.printf "\n" in
    let () = match List.assoc_opt k r with
      | None -> Printf.printf "%i" v
      | Some c -> Printf.printf "%c" c in
    k.y) c 1 in
  Printf.printf "\n\n"

let () = print contraption []

let cache = Hashtbl.create 1_000_000

let end_pos, _ = Contraption.max_binding contraption

type record = {
  cumulative : int;
  pos : coord;
  dir : coord;
  step : int;
}

module Possibilities = Map.Make (struct
  type t = record
  let compare = compare
end)

let rec loop possibles =
  let p, _ = Possibilities.choose possibles in
  let possibles = Possibilities.remove p possibles in
  if p.pos = end_pos
  then p.cumulative
  else
    let hit = Hashtbl.find_opt cache ( p.pos, p.dir, p.step ) in
    if not (hit = None)
    then loop possibles
    else
      let () = Hashtbl.add cache ( p.pos, p.dir, p.step ) true in
      loop (List.fold_left (fun poss dir ->
      if dir <> { x = - p.dir.x; y = - p.dir.y }
        then
          let pos = { x = p.pos.x + dir.x; y = p.pos.y + dir.y } in
          let step = if p.dir = dir then p.step + 1 else 0 in
          match Contraption.find_opt pos contraption with
          | Some cost when step < 3 -> Possibilities.add { cumulative = p.cumulative + cost; pos; dir; step } true poss
          | _ -> poss
        else poss
        ) possibles [ { x = 0; y = 1 }; { x = 0; y = -1 }; { x = 1; y = 0 }; { x = -1; y = 0 }; ])

let part1 = loop ( Possibilities.add { cumulative = 0; pos = { x = 1; y = 1 }; dir = { x = 0; y = 0 }; step = 0 } true (Possibilities.empty) )

let () = Printf.printf "part 1: %i\n" part1


let cache = Hashtbl.create 1_000_000

let rec loop possibles =
  let p, _ = Possibilities.choose possibles in
  let possibles = Possibilities.remove p possibles in
  if p.pos = end_pos && p.step >= 4
  then p.cumulative
  else
    let hit = Hashtbl.find_opt cache ( p.pos, p.dir, p.step ) in
    if not (hit = None)
    then loop possibles
    else
      let () = Hashtbl.add cache ( p.pos, p.dir, p.step ) true in
      let possibles =
        if p.step < 10 && p.dir <> { x = 0; y = 0 } then
        let pos = { x = p.pos.x + p.dir.x; y = p.pos.y + p.dir.y } in
        match Contraption.find_opt pos contraption with
        | Some cost -> Possibilities.add { cumulative = p.cumulative + cost; pos; dir = p.dir; step = p.step + 1 } true possibles
        | _ -> possibles
        else possibles in
      if p.step >= 4 || p.dir = { x = 0; y = 0 } then
      loop (List.fold_left (fun poss dir ->
      if dir <> { x = - p.dir.x; y = - p.dir.y } && dir <> p.dir
        then
          let pos = { x = p.pos.x + dir.x; y = p.pos.y + dir.y } in
          match Contraption.find_opt pos contraption with
          | Some cost -> Possibilities.add { cumulative = p.cumulative + cost; pos; dir; step = 1 } true poss
          | _ -> poss
        else poss
        ) possibles [ { x = 0; y = 1 }; { x = 0; y = -1 }; { x = 1; y = 0 }; { x = -1; y = 0 }; ])
      else loop possibles

let part2 = loop ( Possibilities.add { cumulative = 0; pos = { x = 1; y = 1 }; dir = { x = 0; y = 0 }; step = 0 } true (Possibilities.empty) )

let () = Printf.printf "part 2: %i\n" part2
