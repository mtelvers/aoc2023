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

module Maps = Map.Make (struct
  type t = string
  let compare = compare
end)

let readings = List.fold_left (fun all line ->
  let split = String.split_on_char ' ' line in
  let lst = List.map (int_of_string) (List.rev split) in
  lst :: all) [] read_input

let rec difference_list output = function
  | [] -> output
  | first :: second :: tl ->
    difference_list (output @ [first - second]) (second :: tl)
  | _ :: _ -> output

let next_numbers readings = List.map (fun lst ->
    let rec loop last lst = match (List.fold_left ( + ) 0 lst), lst with
      | 0, _
      | _, [] -> last
      | _, hd :: tl ->
        loop (hd :: last) (difference_list [] (hd :: tl)) in
    List.fold_left ( + ) 0 (loop [] lst)) readings

let sum = List.fold_left ( + ) 0 (next_numbers readings)

let () = Printf.printf "part 1 = %i\n" sum


let readings = List.map (List.rev) readings

let sum = List.fold_left ( + ) 0 (next_numbers readings)

let () = Printf.printf "part 2 = %i\n" sum

