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

let rec follow counter current full_route maps route =
  if current = "ZZZ" then
    counter
  else match route with
  | [] -> follow counter current full_route maps full_route
  | hd :: tl ->
    let l, r = Maps.find current maps in
    follow (counter + 1) (if hd = 'L' then l else r) full_route maps tl

let count = follow 0 "AAA" route maps route

let () = Printf.printf "steps = %i\n" count

