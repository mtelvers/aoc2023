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


let hashmap = Array.init 256 (fun _ -> [])

let () = List.iter (fun seq ->
    let () = match String.index_from_opt seq 0 '=' with
    | Some i ->
      let lens = String.sub seq 0 i in
      let n = int_of_string (String.sub seq (i + 1) (String.length seq - i - 1)) in
      let h = hash lens in
      if List.mem_assoc lens (Array.get hashmap h)
      then Array.set hashmap h (List.map (fun (l, m) -> if l = lens then (l, n) else (l, m)) (Array.get hashmap h))
      else Array.set hashmap h ((Array.get hashmap h) @ [(lens, n)])
    | None -> () in
    match String.index_from_opt seq 0 '-' with
    | Some i ->
      let lens = String.sub seq 0 i in
      let h = hash lens in
      Array.set hashmap h (List.remove_assoc lens (Array.get hashmap h))
    | None -> ()) sequences

let power = Array.mapi (fun i lst ->
    List.mapi (fun j (_, n) ->
        (i + 1) * n * (j + 1)) lst
        |> List.fold_left ( + ) 0) hashmap
    |> Array.fold_left ( + ) 0

let () = Printf.printf "part 2 %i\n" power
