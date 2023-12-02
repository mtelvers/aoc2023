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

let rev s =
  List.init (String.length s) (String.get s)
  |> List.rev
  |> List.map (String.make 1)
  |> String.concat ""

let sum = List.fold_left (fun total line ->
    let rec loop str = 
      match String.length str with
      | 0 -> 0
      | len -> 
        match Nativeint.of_string_opt (String.sub str 0 1) with
        | Some x -> Nativeint.to_int x
        | None -> loop (String.sub str 1 (len - 1))
    in
    let value = 10 * (loop line) + (loop (rev line)) in
    value + total
  ) 0 read_input

let () = Printf.printf "part 1: %i\n" sum

let sum = List.fold_left (fun total line ->
    let nums = [(1, "one"); (2, "two"); (3, "three"); (4, "four"); (5, "five"); (6, "six"); (7, "seven"); (8, "eight"); (9, "nine")] in
    let rev_nums = List.map (fun (num, prefix) -> num, (rev prefix)) nums in
    let rec loop nums str = 
      match String.length str with
      | 0 -> 0
      | len -> 
        match Nativeint.of_string_opt (String.sub str 0 1) with
        | Some x -> Nativeint.to_int x
        | None ->
          let numbers = List.filter_map (fun (num, prefix) -> if String.starts_with ~prefix str then Some num else None) nums in
          match numbers with
          | [] -> loop nums (String.sub str 1 (len - 1))
          | x :: _ -> x
    in
    let value = 10 * (loop nums line) + (loop rev_nums (rev line)) in
    value + total
  ) 0 read_input

let () = Printf.printf "part 2: %i\n" sum
