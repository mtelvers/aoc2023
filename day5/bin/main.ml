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

type range = {
  destination : int;
  source : int;
  length : int;
}

module Maps = Map.Make (struct
  type t = string * string
  let compare = compare
end)

let rec loop lst seeds maps src dst =
  match (lst : Str.split_result list) with
  | [] -> seeds, maps
  | Text "seeds" :: Delim ":" :: tl ->
    let rec loop2 o i =
      match (i : Str.split_result list) with
      | [] -> o, i
      | Text s :: tl ->
        (match Int.of_string_opt s with
        | Some n -> loop2 (n :: o) tl
        | None -> o, i)
      | Delim _ :: tl -> loop2 o tl in
    let seeds, tl = loop2 [] tl in
    loop tl seeds maps src dst
  | Text src :: Delim "-" :: Text "to" :: Delim "-" :: Text dst :: Delim " " :: Text "map" :: Delim ":" :: tl ->
    loop tl seeds maps src dst
  | Text n1 :: Delim " " :: Text n2 :: Delim " " :: Text n3 :: tl ->
    let () = Printf.printf "src >%s< dst >%s<\n" src dst in
    let l = match Maps.find_opt (src, dst) maps with
      | Some v -> v
      | None -> [] in
    let l = l @ [{ destination = int_of_string n1; source = int_of_string n2; length = int_of_string n3 }] in
    let maps = Maps.add (src, dst) l maps in
    loop tl seeds maps src dst
  | Text s :: tl ->
    let () = Printf.printf "unhandled %s\n" s in
    loop tl seeds maps src dst
  | Delim _ :: tl ->
    loop tl seeds maps src dst

let seeds, maps =
  let tokens = List.rev read_input |> List.map (Str.full_split (Str.regexp "[ :-]")) |> List.flatten in
  loop tokens [] Maps.empty "" ""

let () = List.iter (Printf.printf "%i,") seeds
let () = Printf.printf "\n"

let () = Maps.iter (fun k v ->
    let s, d = k in
    let () = Printf.printf "(%s, %s) = " s d in
    let () = List.iter (fun r -> Printf.printf "d %i s %i r %i" r.destination r.source r.length) v in
    Printf.printf "\n"
  ) maps

let rec lookup n src =
  let (_, d), ranges = Maps.choose (Maps.filter (fun (s, _) _ -> s = src) maps) in
  let r = List.filter (fun map -> n >= map.source && n <= (map.source + map.length - 1)) ranges in
  let n = if List.length r > 0
    then let m = List.hd r
      in m.destination + (n - m.source)
    else
      n in
  if d = "location"
  then n
  else lookup n d

let locations = List.map (fun seed -> lookup seed "seed") seeds

let nearest = List.fold_left (fun m l -> min m l) (List.hd locations) locations

let () = List.iter (Printf.printf "%i,") locations
let () = Printf.printf "\n"

let () = Printf.printf "nearest = %i\n" nearest


let nearest =
  let rec locations_helper x = function
    | []
    | _ :: [] -> x
    | start :: length :: tl ->
let () = Printf.printf "range start %i of length %i\n" start length in
let () = flush stdout in
      let rec loop l = function
        | 0 -> l
        | n -> loop (min (lookup (start + (n - 1)) "seed") l) (n - 1) in
    locations_helper (loop x length) tl
  in
  locations_helper Int.max_int (List.rev seeds)

let () = Printf.printf "nearest = %i\n" nearest

