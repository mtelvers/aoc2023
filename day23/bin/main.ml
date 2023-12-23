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

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

type coord = {
  y : int;
  x : int;
}

module Island = Map.Make (struct
  type t = coord
  let compare = compare
end)

let island, _ = List.fold_left (fun (island, location) line ->
    String.fold_left (fun (island, location) ch ->
      (Island.add location ch island), { x = location.x + 1; y = location.y }
    ) (island, { x = 1; y = location.y + 1 }) line
  ) (Island.empty, { x = 1; y = 0 }) (List.rev read_input)

let neighbours p =
  [
    { x = p.x    ; y = p.y + 1 };
    { x = p.x - 1; y = p.y     };
    { x = p.x + 1; y = p.y     };
    { x = p.x    ; y = p.y - 1 };
  ]

let print ?(route=[]) f =
  let _ = Island.fold (fun k v y ->
    let () = if k.y != y then Printf.printf "\n" in
    let () = Printf.printf "%c" (if List.mem k route then 'O' else v) in
    k.y) f 1 in
  Printf.printf "\n\n"

let () = print island

let tl, _ = Island.min_binding island
let start = { x = tl.x + 1; y = tl.y }
let br, _ = Island.max_binding island
let finish = { x = br.x - 1; y = br.y }

let rec loop route =
  let pos = List.hd route in
  if pos = finish
  then ((List.length route) - 1)
  else 
    let options = neighbours pos in
    let options = List.filter (fun p ->
      not (List.mem p route) &&
      match Island.find_opt p island with
      | None -> false
      | Some '#' -> false
      | Some '.' -> true
      | Some '>' when p.x - 1 = pos.x -> true
      | Some '<' when p.x + 1 = pos.x -> true
      | Some '^' when p.y + 1 = pos.y -> true
      | Some 'v' when p.y - 1 = pos.y -> true
      | _ -> false
    ) options in
    List.fold_left (fun longest p -> max longest (loop (p :: route))) 0 options

let part1 = loop [start]

let () = Printf.printf "part 1 = %i\n" part1
