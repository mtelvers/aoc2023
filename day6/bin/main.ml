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

module Int = struct
  include Int
  let of_string_opt s =
    try Some (int_of_string s)
    with Failure _ -> None
end

let rec loop2 o i =
  match (i : Str.split_result list) with
  | [] -> o
  | Text s :: tl ->
    (match Int.of_string_opt s with
    | Some n -> loop2 (o @ [n]) tl
    | None -> o)
  | Delim _ :: tl -> loop2 o tl

let rec loop lst times distances =
  match (lst : Str.split_result list) with
  | [] -> times, distances
  | Text "Time" :: Delim ":" :: tl ->
    let times = loop2 [] tl in
    loop [] times distances
  | Text "Distance" :: Delim ":" :: tl ->
    let distances = loop2 [] tl in
    loop [] times distances
  | Text _ :: tl -> loop tl times distances
  | Delim _ :: tl -> loop tl times distances

let times, distances = List.fold_left (fun (times, distances) line ->
  let split = Str.full_split (Str.regexp "[: ]") line in
  loop split times distances) ([], []) read_input

let quadratic a b c =
  let m = Float.sqrt ( b *. b -. 4. *. a *. c ) in
  let x1 = ( -. b +. m ) /. (2. *. a) in
  let x2 = ( -. b -. m ) /. (2. *. a) in
  (x1, x2)

let always_round_down x =
  let i = int_of_float x in
  if (float_of_int i) = x then (i - 1) else i

let solutions = List.map2 (fun t d ->
  let x1, x2 = quadratic (-1.) (float_of_int t) (float_of_int (-d)) in
  let x1 = 1 + int_of_float x1 in
  let x2 = always_round_down x2 in
  let n = x2 - x1 + 1 in
  n)

let product = List.fold_left ( * ) 1 (solutions times distances)

let () = Printf.printf "product = %i\n" product

let rec fix_kerning = function
  | [] -> ""
  | hd :: tl -> string_of_int hd ^ (fix_kerning tl)

let times = [int_of_string (fix_kerning times)]
let distances = [int_of_string (fix_kerning distances)]

let product = List.fold_left ( * ) 1 (solutions times distances)

let () = Printf.printf "product = %i\n" product
