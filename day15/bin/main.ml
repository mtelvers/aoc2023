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

let sequences =
  let input = String.concat "," (List.rev read_input) in
  String.split_on_char ',' input

let hash s =
  String.fold_left (fun h c -> (17 * (h + (Char.code c))) mod 256) 0 s

let part1 =
  List.fold_left (fun sum s -> sum + (hash s)) 0 sequences

let () = Printf.printf "part 1 %i\n" part1
