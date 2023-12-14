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

module Platform = Map.Make (struct
  type t = coord
  let compare = compare
end)

let platform, _ = List.fold_left (fun (platform, location) line ->
    String.fold_left (fun (platform, location) ch ->
      (Platform.add location ch platform), { x = location.x + 1; y = location.y }
    ) (platform, { x = 1; y = location.y + 1 }) line
  ) (Platform.empty, { x = 1; y = 0 }) (List.rev read_input)

let print p =
  let _ = Platform.fold (fun k v y ->
    let () = if k.y != y then Printf.printf "\n" in
    let () = Printf.printf "%c" v in
    k.y) p 1 in
  Printf.printf "\n\n"

let north pos = { x = pos.x; y = pos.y - 1 }
let west pos = { x = pos.x - 1; y = pos.y }
let south pos = { x = pos.x; y = pos.y + 1 }
let east pos = { x = pos.x + 1; y = pos.y }

let can_move p r dir =
  match Platform.find_opt (dir r) p with
  | None -> false
  | Some '.' -> true
  | Some _ -> false

let () = print platform

let rec tip p dir =
  let rocks = Platform.filter (fun r ch -> ch = 'O' && can_move p r dir) p in
  if Platform.cardinal rocks > 0
  then
    let p = Platform.fold (fun pos _ a -> Platform.add pos '.' a |> Platform.add (dir pos) 'O' ) rocks p in
    tip p dir
  else p

let load p =
  let y, c, count = Platform.fold (fun k v (y, c, count) ->
    let count, c = if k.y != y then (count @ [c]), 0 else count, c in
    let c = if v = 'O' then c + 1 else c in
    k.y, c, count) p (1, 0, []) in
  let count = count @ [c] in
  let sum, _ = List.fold_left (fun (sum, y) v -> sum + v * y, y - 1) (0, y) count in
  sum

let () = Printf.printf "part 1 %i\n" (load (tip platform north))


let cache = Hashtbl.create 1_000
let results = Hashtbl.create 1_000

let rec loop n p =
  match Hashtbl.find_opt cache p with
    | Some x -> (x, n)
    | None ->
      let () = Hashtbl.add results n (load p) in
      let () = Hashtbl.add cache p n in
      let p = List.fold_left (fun p dir -> tip p dir) p [ north; west; south; east ] in
      loop (n + 1) p

let entry, last = loop 0 platform

let final = entry + ((1_000_000_000 - entry) mod (last - entry))
let () = Printf.printf "part 2 %i\n" (Hashtbl.find results final)
