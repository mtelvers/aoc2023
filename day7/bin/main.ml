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

let face_value ?(wild = true) = function
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'T' -> 10
  | 'J' -> if wild then 1 else 11
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | _ -> 0

let games, bids = List.fold_left (fun (games, bids) line ->
  let split = String.split_on_char ' ' line in
  let cards = List.hd split in
  let cards = List.map (face_value) (explode cards) in
  let bid = int_of_string (List.hd (List.rev split)) in
  (cards :: games, bid :: bids)) ([], []) read_input

let rec runs (last:int) (results:int list list) = function
  | [] -> results
  | hd :: tl ->
    if hd = last
    then runs hd ((hd :: (List.hd results)) :: (List.tl results)) tl
    else runs hd ([hd] :: results) tl

let rank = function
  | 5 :: _ -> 7
  | 1 :: 4 :: _ -> 6
  | 2 :: 3 :: _ -> 5
  | 1 :: 1 :: 3 :: _ -> 4
  | 1 :: 2 :: 2 :: _ -> 3
  | 1 :: 1 :: 1 :: 2 :: _ -> 2
  | 1 :: 1 :: 1 :: 1 :: 1 :: _ -> 1
  | tl ->
    let () = List.iter (Printf.printf "%i\n") tl in
    assert false

let ranked_games = List.map2 (fun g b ->
  let sorted = List.sort (compare) g in
  let groups = runs 0 [] sorted in
  let run_lengths = List.map (List.length) groups in
  rank (List.sort (compare) run_lengths), g, b) games bids

let order = List.sort (fun (rank1, cards1, _) (rank2, cards2, _) ->
  let r = compare rank1 rank2 in
  if r = 0 then List.compare (compare) cards1 cards2
  else r)

let sum = List.mapi (fun i (_, _, b) -> (i + 1) * b) (order ranked_games)

let total = List.fold_left ( + ) 0 sum

let () = Printf.printf "part 1 = %i\n" total





let games, bids = List.fold_left (fun (games, bids) line ->
  let split = String.split_on_char ' ' line in
  let cards = List.hd split in
  let cards = List.map (face_value ~wild:true) (explode cards) in
  let bid = int_of_string (List.hd (List.rev split)) in
  (cards :: games, bid :: bids)) ([], []) read_input

let ranked_games = List.map2 (fun g b ->
  let jokers, others = List.partition (fun v -> v = 1) g in
  let njokers = List.length jokers in
  let sorted = List.sort (compare) others in
  let groups = runs 0 [] sorted in
  let run_lengths = List.map (List.length) groups in
  let sorted_run_lengths = List.sort (compare) run_lengths in
  let sorted_run_lengths = if njokers > 0
    then match List.rev sorted_run_lengths with
      | [] -> [njokers]
      | best :: rest -> List.rev ((best + njokers) :: rest)
    else sorted_run_lengths in
  rank sorted_run_lengths, g, b) games bids

let sum = List.mapi (fun i (_, _, b) -> (i + 1) * b) (order ranked_games)

let total = List.fold_left ( + ) 0 sum

let () = Printf.printf "part 2 = %i\n" total

