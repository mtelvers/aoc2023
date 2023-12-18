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

type record = {
  dir : char;
  dist : int;
  colour : int;
}

let records = List.fold_left (fun lst line ->
    (Scanf.sscanf line " %c %i (#%x) " (fun dir dist colour -> { dir; dist; colour })) :: lst
  ) [] read_input

let () = List.iter (fun v -> Printf.printf "%c %i %x\n" v.dir v.dist v.colour) records

type coord = {
  y : int;
  x : int;
}

module Lagoon = Map.Make (struct
  type t = coord
  let compare = compare
end)

let _, lagoon = List.fold_left (fun (o, l) r ->
    let vec = match r.dir with
      | 'R' -> { x = 1; y = 0; }
      | 'L' -> { x = -1; y = 0; }
      | 'U' -> { x = 0; y = -1; }
      | 'D' -> { x = 0; y = 1; }
      | _ -> assert false in
    let rec loop pos lag = function
      | 0 -> pos, lag
      | n ->
        let pos = { x = pos.x + vec.x; y = pos.y + vec.y } in
        let () = Printf.printf "add %i,%i\n" pos.x pos.y in
        loop pos (Lagoon.add pos '#' lag) (n - 1) in
    loop o l r.dist
  ) ({ x = 0; y = 0 }, Lagoon.empty) records

let any, _ = Lagoon.choose lagoon
let top_left = Lagoon.fold (fun k _ a -> { x = min k.x a.x; y = min k.y a.y }) lagoon any
let bottom_right = Lagoon.fold (fun k _ a -> { x = max k.x a.x; y = max k.y a.y }) lagoon any
let () = Printf.printf "top left %i,%i " top_left.x top_left.y
let () = Printf.printf "bottom right %i,%i\n" bottom_right.x bottom_right.y

let () = for y = top_left.y to bottom_right.y do
  for x = top_left.x to bottom_right.x do
    match Lagoon.find_opt {x;y} lagoon with
    | None -> Printf.printf " "
    | Some c -> Printf.printf "%c" c
  done;
  Printf.printf "\n";
  done

let rec fill lag o =
  let neighbours o = [ { x = o.x + 1; y = o.y }; { x = o.x - 1; y = o.y }; { x = o.x; y = o.y + 1 }; { x = o.x; y = o.y - 1 } ] in
  List.fold_left (fun l pos ->
      match Lagoon.find_opt pos l with
      | None -> fill (Lagoon.add pos '#' l) pos
      | Some _ -> l) lag (neighbours o)

let mid a b =
  (abs (a + b)) / 2

let lagoon = fill lagoon { x = mid bottom_right.x top_left.x; y = mid bottom_right.y top_left.y }

let () = for y = top_left.y to bottom_right.y do
  for x = top_left.x to bottom_right.x do
    match Lagoon.find_opt {x;y} lagoon with
    | None -> Printf.printf " "
    | Some c -> Printf.printf "%c" c
  done;
  Printf.printf "\n";
  done

let () = Printf.printf "part 1 %i\n" (Lagoon.cardinal lagoon)

