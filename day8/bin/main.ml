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

let rec gcd u v =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)

let explode s = List.init (String.length s) (String.get s)

module Maps = Map.Make (struct
  type t = string
  let compare = compare
end)

let route, maps = List.fold_left (fun (route, maps) line ->
  let split = Str.full_split (Str.regexp "[ (),=]") line in
  match split with
    | [] -> route, maps
    | Text n1 :: Delim " " :: Delim "=" :: Delim " " ::
      Delim "(" :: Text n2 :: Delim "," :: Delim " " ::
      Text n3 :: Delim ")" :: _ -> route, (Maps.add n1 (n2, n3) maps)
    | Text r :: _ -> (explode r), maps
    | _ -> route, maps) ([], Maps.empty) read_input

let ends = ["ZZZ"]

let rec follow counter current ends rt =
  if List.mem current ends then
    counter
  else match rt with
  | [] -> follow counter current ends route
  | hd :: tl ->
    let l, r = Maps.find current maps in
    follow (counter + 1) (if hd = 'L' then l else r) ends tl

let count = follow 0 "AAA" ends route

let () = Printf.printf "steps = %i\n" count

let starts, ends = Maps.fold (fun k _ (s, e) ->
  match String.get k 2 with
    | 'A' -> (k :: s, e)
    | 'Z' -> (s, k :: e)
    | _ -> (s, e)) maps ([], [])

let lengths = List.map (fun start -> follow 0 start ends route) starts

let part2 = List.fold_left (lcm) 1 lengths

let () = Printf.printf "part2 = %i\n" part2

