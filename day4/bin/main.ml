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

  let rec pow a = function
    | 0 -> 1
    | 1 -> a
    | n ->
      let b = pow a (n / 2) in
      b * b * (if n mod 2 = 0 then 1 else a)
end

type card = {
  winners : int list;
  numbers : int list;
  copies : int;
}

module Cards = Map.Make (struct
  type t = int
  let compare = compare
end)

type kind = Card | Winner | Number

let rec loop lst n k card =
  match (lst : Str.split_result list) with
  | [] -> n, card
  | Text "Card" :: tl ->
    loop tl n k card
  | Text s :: tl ->
    let n, c = match k with
    | Card -> (int_of_string s), card
    | Winner -> n, { winners = (int_of_string s) :: card.winners; numbers = card.numbers; copies = 1 }
    | Number -> n, { winners = card.winners; numbers = (int_of_string s) :: card.numbers ; copies = 1 } in
    loop tl n k c
  | Delim ":" :: tl -> loop tl n Winner card
  | Delim "|" :: tl -> loop tl n Number card
  | Delim _ :: tl -> loop tl n k card

let cards = List.fold_left (fun cards line ->
  let split = Str.full_split (Str.regexp "[:| ]") line in
  let n, card = loop split 0 Card { winners = []; numbers = []; copies = 0 } in
  Cards.add n card cards) Cards.empty read_input

let sum = Cards.fold (fun _ card sum ->
  let w = List.fold_left (fun wins num ->
      if List.mem num card.winners
      then wins + 1
      else wins
    ) 0 card.numbers in
  let score = if w > 0 then Int.pow 2 (w - 1) else 0 in
  sum + score) cards 0

let () = Printf.printf "part 1: %i\n" sum


let rec process num cards =
  match Cards.find_opt num cards with
  | None -> cards
  | Some card -> 
    let w = List.fold_left (fun wins num ->
      if List.mem num card.winners
      then wins + 1
      else wins) 0 card.numbers in
    let rec loop cards = function
      | 0 -> cards
      | n ->
        let c = Cards.find (num + n) cards in
        let cards = Cards.add (num + n) { winners = c.winners; numbers = c.numbers; copies = c.copies + card.copies } cards in
        loop cards (n - 1) in
    let () = Printf.printf "wins %i\n" w in
    let cards = loop cards w in
    let () = Cards.iter (fun k v -> Printf.printf "%i=%i," k v.copies) cards in
    let () = Printf.printf "\n" in
    let () = flush stdout in
    process (num + 1) cards

let cards = process 1 cards

let sum = Cards.fold (fun _ card sum -> sum + card.copies) cards 0

let () = Printf.printf "part 2: %i\n" sum
