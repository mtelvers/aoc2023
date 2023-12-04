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
    | Winner -> n, { winners = (int_of_string s) :: card.winners; numbers = card.numbers }
    | Number -> n, { winners = card.winners; numbers = (int_of_string s) :: card.numbers } in
    loop tl n k c
  | Delim ":" :: tl -> loop tl n Winner card
  | Delim "|" :: tl -> loop tl n Number card
  | Delim _ :: tl -> loop tl n k card

let cards = List.fold_left (fun cards line ->
  let split = Str.full_split (Str.regexp "[:| ]") line in
  let n, card = loop split 0 Card { winners = []; numbers = [] } in
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


