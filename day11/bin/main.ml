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

module Universe = Map.Make (struct
  type t = coord
  let compare = compare
end)

let universe, _ = List.fold_left (fun (universe, location) line ->
    String.fold_left (fun (universe, location) ch ->
      (Universe.add location ch universe), { x = location.x + 1; y = location.y }
    ) (universe, { x = 1; y = location.y + 1 }) line
  ) (Universe.empty, { x = 1; y = 0 }) (List.rev read_input)

let galaxies, _ = Universe.filter (fun _ ch -> ch = '#') universe
             |> Universe.to_list
             |> List.split

let expansion_x, expansion_y =
  let max, _ = Universe.max_binding universe in
  let min, _ = Universe.min_binding universe in
  let x = List.init (max.x - min.x) (fun x -> x + min.x) in
  let y = List.init (max.y - min.y) (fun y -> y + min.y) in
  let x = List.filter (fun x ->
      not (List.fold_left (fun a g ->
          if g.x = x then true else a) false galaxies)) x in
  let y = List.filter (fun y ->
      not (List.fold_left (fun a g ->
          if g.y = y then true else a) false galaxies)) y in
  x, y

let apply_expansion g comp dist =
  List.fold_left (fun lst v ->
      List.map (fun p -> if comp p v
                 then { x = p.x + dist.x; y = p.y + dist.y }
                 else p) lst
  ) g

let galaxies = apply_expansion galaxies (fun p1 p2 -> p1.x > p2) { x = 1; y = 0 } (List.rev expansion_x)
let galaxies = apply_expansion galaxies (fun p1 p2 -> p1.y > p2) { x = 0; y = 1 } (List.rev expansion_y)

let rec loop sum = function
  | [] -> sum
  | hd :: tl ->
    let sum = List.fold_left (fun sum g ->
        sum + abs (g.y - hd.y) + abs (g.x - hd.x)) sum tl in
    loop sum tl

let sum = loop 0 galaxies

let () = Printf.printf "part 1: %i\n" sum



let galaxies, _ = Universe.filter (fun _ ch -> ch = '#') universe
             |> Universe.to_list
             |> List.split

let galaxies = apply_expansion galaxies (fun p1 p2 -> p1.x > p2) { x = 999999; y = 0 } (List.rev expansion_x)
let galaxies = apply_expansion galaxies (fun p1 p2 -> p1.y > p2) { x = 0; y = 999999 } (List.rev expansion_y)

let sum = loop 0 galaxies

let () = Printf.printf "part 2: %i\n" sum

