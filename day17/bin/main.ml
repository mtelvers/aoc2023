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

(*
let loop o =
  let neighbours p = [ { x = p.x + 1; y = p.y     }; { x = p.x - 1; y = p.y     }; { x = p.x    ; y = p.y + 1 }; { x = p.x    ; y = p.y - 1 }; ] in
  List.fold_left (fun a n ->
      match Contraption.find_opt n a with
      | None ->
      | Some v ->
    ) contraption (neighbours o)
*)

let cache = Hashtbl.create 1_000_000

(*
exception Found
exception Too_Long

let rec run input_pos last_dir steps length best route =
    let directions = List.filter (fun d -> not (last_dir.x = - d.x && last_dir.y = - d.y)) [ { x = 0; y = 1 }; { x = 0; y = -1 }; { x = 1; y = 0 }; { x = -1; y = 0 }; ] in
(*      let () = Printf.printf "[H" in *)
      let () = print contraption route in
      let () = Printf.printf "%i %i (route length %i)\n" best length (List.length route) in
      let () = flush stdout in
      List.fold_left (fun (suc, b) d ->
        try
          let output_pos = { x = input_pos.x + d.x; y = input_pos.y + d.y } in
          let cost = Contraption.find output_pos contraption in
          let () = if length + cost > best then raise (Too_Long) in
      (*    let () = if (List.length route > 500) then raise (Too_Long) in *)
          let s = if d = last_dir then steps + 1 else 0 in
          let () = if s > 3 then raise (Too_Long) in
          let max, _ = Contraption.max_binding contraption in
          let () = if max = output_pos then raise (Found) in
          match Hashtbl.find_opt cache ( output_pos, d, steps ) with
          | Some _ -> (suc, b)
          | None ->
            let route = match d with
              | { x = 0; y = 1 } -> (output_pos, 'v') :: route
              | { x = 0; y = -1 } -> (output_pos, '^') :: route
              | { x = -1; y = 0 } -> (output_pos, '<') :: route
              | { x = 1; y = 0 } -> (output_pos, '>') :: route
              | _ -> assert false
          in
          let success, b2 = run output_pos d s (length + cost) b route in
          let () = if success then Printf.printf "%c %i\n" (if success then 'T' else 'F') b2 in
          let () = Hashtbl.add cache ( output_pos, d, s ) ( success, b2 ) in
          (success || suc, min b2 b)
        with
          | Too_Long
          | Not_found -> false, best
          | Found -> true, length + cost
        ) (false, best) directions
          *)

(*
let rec run input_pos last_dir steps length best =
   (* let () = print contraption [] in
      let () = Printf.printf "pos (%i,%i) dir (%i,%i) %i %i %i\n" input_pos.x input_pos.y last_dir.x last_dir.y best length (Hashtbl.length cache) in
      let () = flush stdout in
      *)
      List.fold_left (fun (suc, b) d ->
        if last_dir.x = - d.x && last_dir.y = - d.y
        then (suc, b)
        else
          let hit = Hashtbl.find_opt cache ( input_pos, d ) in
          if not (hit = None)
          then let () = Printf.printf "miss\n" in (suc, b)
          else
            let () = Printf.printf "hit\n" in
            let new_pos = { x = input_pos.x + d.x; y = input_pos.y + d.y } in
            let cost = Contraption.find_opt new_pos contraption in
            if cost = None
            then (suc, b)
            else
              let length = length + (Option.value ~default:0 cost) in
              if length > best
              then (suc, b)
              else
                let s = if d = last_dir then steps + 1 else 0 in
                if s >= 3
                then (suc, b)
                else
                  let max, _ = Contraption.max_binding contraption in
                  if max = new_pos
                  then (true, length)
                  else
                    let () = Hashtbl.add cache ( new_pos, d ) s in
                    let success, result = run new_pos d s length b in
                    let () = if success then Printf.printf "%c %i\n" (if success then 'T' else 'F') result in
                    (success || suc, if success then min result b else b)
          ) (false, best) [ { x = 0; y = 1 }; { x = 0; y = -1 }; { x = 1; y = 0 }; { x = -1; y = 0 }; ]

let success, part1 = run { x = 1; y = 1} { x = 0; y = 0 } 0 0 1000 (*(13 * 13 * 9)*)
    *)

let end_pos, _ = Contraption.max_binding contraption

type record = {
  cumulative : int;
  pos : coord;
  dir : coord;
  step : int;
  c : int list;
}

module Possibilities = Map.Make (struct
  type t = record
  let compare = compare
end)

let rec loop possibles =
  let p, _ = Possibilities.choose possibles in
  let possibles = Possibilities.remove p possibles in
  if p.pos = end_pos
  then
    let possibles = Possibilities.add p true possibles in
    let () = Possibilities.iter (fun k _ ->
        let () = Printf.printf "%i %i,%i %i,%i %i : " k.cumulative k.pos.x k.pos.y k.dir.x k.dir.y k.step in
        let () = List.iter (Printf.printf "%i,") k.c in
        Printf.printf "\n"
      ) possibles in p.cumulative
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
          | Some cost when step < 3 -> Possibilities.add { cumulative = p.cumulative + cost; pos; dir; step; c = p.c  @ [ cost ]} true poss
          | _ -> poss
        else poss
        ) possibles [ { x = 0; y = 1 }; { x = 0; y = -1 }; { x = 1; y = 0 }; { x = -1; y = 0 }; ])

let part1 = loop ( Possibilities.add { cumulative = 0; pos = { x = 1; y = 1 }; dir = { x = 0; y = 0 }; step = 0; c = [] } true (Possibilities.empty) )

let () = Printf.printf "part 1: %i\n" part1


let cache = Hashtbl.create 1_000_000

let rec loop possibles =
  let p, _ = Possibilities.choose possibles in
  let possibles = Possibilities.remove p possibles in
  if p.pos = end_pos && p.step >= 4
  then
    let possibles = Possibilities.add p true possibles in
    let () = Possibilities.iter (fun k _ ->
        let () = Printf.printf "%i %i,%i %i,%i %i : " k.cumulative k.pos.x k.pos.y k.dir.x k.dir.y k.step in
        let () = List.iter (Printf.printf "%i,") k.c in
        Printf.printf "\n"
      ) possibles in p.cumulative
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
        | Some cost -> Possibilities.add { cumulative = p.cumulative + cost; pos; dir = p.dir; step = p.step + 1; c = p.c @ [ cost ] } true possibles
        | _ -> possibles
        else possibles in
      if p.step >= 4 || p.dir = { x = 0; y = 0 } then
      loop (List.fold_left (fun poss dir ->
      if dir <> { x = - p.dir.x; y = - p.dir.y } && dir <> p.dir
        then
          let pos = { x = p.pos.x + dir.x; y = p.pos.y + dir.y } in
          match Contraption.find_opt pos contraption with
          | Some cost -> Possibilities.add { cumulative = p.cumulative + cost; pos; dir; step = 1; c = p.c @ [ cost ] } true poss
          | _ -> poss
        else poss
        ) possibles [ { x = 0; y = 1 }; { x = 0; y = -1 }; { x = 1; y = 0 }; { x = -1; y = 0 }; ])
      else loop possibles

let part2 = loop ( Possibilities.add { cumulative = 0; pos = { x = 1; y = 1 }; dir = { x = 0; y = 0 }; step = 0; c = [] } true (Possibilities.empty) )

let () = Printf.printf "part 2: %i\n" part2

(*
let beams = [{ x = 1; y = 1 }, { x = 1; y = 0 }]

let rec loop lst = function
  | [] -> lst
  | (pos, dir) :: tl ->
    let new_pos = { x = pos.x + dir.x; y = pos.y + dir.y } in
    match Contraption.find_opt new_pos contraption with
    | None -> loop lst tl
    | Some _ ->
      match dir.x, dir.y with
      | 0, 1 -> loop (lst @ [new_pos, { x = 0; y = 1 }; new_pos, { x = -1; y = 0 }; new_pos, { x = 1; y = 0 }]) tl
      | 0, -1 -> loop (lst @ [new_pos, { x = 0; y = -1 }; new_pos, { x = -1; y = 0 }; new_pos, { x = 1; y = 0 }]) tl
      | 1, 0 -> loop (lst @ [new_pos, { x = 1; y = 0 }; new_pos, { x = 0; y = -1 }; new_pos, { x = 0; y = 1 }]) tl
      | -1, 0 -> loop (lst @ [new_pos, { x = -1; y = 0 }; new_pos, { x = 0; y = -1 }; new_pos, { x = 0; y = 1 }]) tl
      | _ -> assert false

let cache = Hashtbl.create 1_000_000

let rec run beams energized =
  let beams = loop [] beams in
  let () = Printf.printf "# %i" (List.length beams) in
  let beams = List.filter (fun (pos, _) -> not (Hashtbl.mem cache ( pos ))) beams in
  let () = List.iter (fun (pos, _) -> Hashtbl.add cache ( pos ) true) beams in
  let () = Printf.printf " filtered to %i hash table %i\n" (List.length beams) (Hashtbl.length cache) in
  let () = List.iter (fun (pos, dir) -> Printf.printf "%i,%i %i,%i\n" pos.x pos.y dir.x dir.y) beams in
  let () = print contraption beams in
  let () = flush stdout in
  let ne = Contraption.cardinal energized in
  if List.length beams > 0
  then run beams energized
  else ne

let part1 = run beams Contraption.empty
let () = Printf.printf "part 1: %i\n" part1
*)
