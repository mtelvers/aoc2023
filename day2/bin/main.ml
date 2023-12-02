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

type draw = {
  red : int;
  green : int;
  blue : int;
}

module Games = Map.Make (struct
  type t = int
  let compare = compare
end)

let rec loop lst n draw draws =
  match (lst : Str.split_result list) with
  | [] -> 
    n, draw :: draws
  | Text "Game" :: Delim " " :: Text s :: Delim ":" :: tl ->
    loop tl (int_of_string s) draw draws
  | Text s :: Delim " " :: Text "red" :: tl ->
    loop tl n { red = int_of_string s; green = draw.green; blue = draw.blue } draws
  | Text s :: Delim " " :: Text "green" :: tl ->
    loop tl n { red = draw.red; green = int_of_string s; blue = draw.blue } draws
  | Text s :: Delim " " :: Text "blue" :: tl ->
    loop tl n { red = draw.red; green = draw.green; blue = int_of_string s } draws
  | Text _ :: tl ->
    loop tl n draw draws
  | Delim ";" :: tl ->
    loop tl n { red = 0; green = 0; blue = 0 } (draw :: draws)
  | Delim _ :: tl ->
    loop tl n draw draws

let games = List.fold_left (fun games line ->
  let split = Str.full_split (Str.regexp "[;,: ]") line in
  let num, draws = loop split 0 { red = 0; green = 0; blue = 0 } [] in
  Games.add num draws games) Games.empty read_input

let bag = { red = 12; green = 13; blue = 14 }

let sum_possible = Games.fold (fun num draws sum ->
  let valid = List.fold_left (fun valid draw ->
    if draw.red > bag.red || draw.green > bag.green || draw.blue > bag.blue
    then false && valid
    else true && valid) true draws in
  if valid then sum + num else sum) games 0

let () = Printf.printf "part 1: %i\n" sum_possible

let sum_of_power = Games.fold (fun _ draws sum ->
  let bag = List.fold_left (fun super draw ->
    { red = max super.red draw.red; green = max super.green draw.green; blue = max super.blue draw.blue }
    ) { red = 0; green = 0; blue = 0 } draws in
  let power = bag.red * bag.green * bag.blue in
  sum + power) games 0

let () = Printf.printf "part 2: %i\n" sum_of_power

