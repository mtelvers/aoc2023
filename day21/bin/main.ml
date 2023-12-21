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

module Gardens = Map.Make (struct
  type t = coord
  let compare = compare
end)

let gardens, _ = List.fold_left (fun (gardens, location) line ->
    String.fold_left (fun (gardens, location) ch ->
      (Gardens.add location ch gardens), { x = location.x + 1; y = location.y }
    ) (gardens, { x = 1; y = location.y + 1 }) line
  ) (Gardens.empty, { x = 1; y = 0 }) (List.rev read_input)

let neighbours p =
  [
    { x = p.x    ; y = p.y + 1 };
    { x = p.x - 1; y = p.y     };
    { x = p.x + 1; y = p.y     };
    { x = p.x    ; y = p.y - 1 };
  ]

let print g =
  let _ = Gardens.fold (fun k v y ->
    let () = if k.y != y then Printf.printf "\n" in
    let () = Printf.printf "%c" v in
    k.y) g 1 in
  Printf.printf "\n\n"

let () = print gardens

let start = Gardens.fold (fun k v start -> if v = 'S' then k else start) gardens { x = 1; y = 1 }

let rec expand = function
  | [] -> []
  | hd :: tl -> (List.filter (fun p ->
      match Gardens.find_opt p gardens with
      | Some 'S'
      | Some '.' -> true
      | _ -> false
      ) (neighbours hd)) @ expand tl

let rec loop lst = function
  | 0 -> lst
  | n -> loop (List.sort_uniq (compare) (expand lst)) (n - 1)

let l = loop [start] 64

let () = List.iter (fun p -> Printf.printf "(%i,%i)" p.x p.y) l
let () = Printf.printf "\n"

let () = Printf.printf "part 1 = %i\n" (List.length l)
