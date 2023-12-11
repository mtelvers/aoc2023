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

let () = List.iter (Printf.printf "%i\n") expansion_x
let () = List.iter (Printf.printf "%i\n") expansion_y

let apply_expansion_x x g =
  List.fold_left (fun lst x ->
      List.map (fun p -> if p.x > x
                 then { x = p.x + 1; y = p.y }
                 else p) lst
  ) g (List.rev x)

let apply_expansion_y y g =
  List.fold_left (fun lst y ->
      List.map (fun p -> if p.y > y
                 then { x = p.x; y = p.y + 1 }
                 else p) lst
  ) g (List.rev y)

let () = Printf.printf "before\n"
let () = List.iter (fun p -> Printf.printf "%i,%i\n" p.x p.y) galaxies

let galaxies = apply_expansion_x expansion_x galaxies
let galaxies = apply_expansion_y expansion_y galaxies

let () = Printf.printf "after\n"
let () = List.iter (fun p -> Printf.printf "%i,%i\n" p.x p.y) galaxies

let rec loop sum = function
  | [] -> sum
  | hd :: tl ->
    let () = Printf.printf "head %i,%i" hd.x hd.y in
    let sum = List.fold_left (fun sum g ->
        let dist = abs (g.y - hd.y) + abs (g.x - hd.x) in
        let () = Printf.printf "to %i,%i is %i\n" g.x g.y dist in
        sum + dist) sum tl in
    loop sum tl

let sum = loop 0 galaxies

let () = Printf.printf "part 1: %i\n" sum
