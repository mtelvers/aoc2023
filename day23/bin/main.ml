open Bigarray

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

type coord = {
  y : int;
  x : int;
}

let read_input = List.rev read_input
let length = String.length (List.hd read_input)

let island = Array2.init Char C_layout length length (fun x y -> String.get (List.nth read_input y) x)

let print ?(route=[]) i =
  let () = Printf.printf "print\n" in
    for y = 0 to length - 1 do
      for x = 0 to length - 1 do
        Printf.printf "%c" (if List.mem { x; y } route then 'O' else i.{ x, y })
      done ;
      Printf.printf "\n" ;
    done ;
    Printf.printf "\n"

let () = print island

let start = { x = 1; y = 0 }
let finish = { x = length - 2; y = length - 1 }

let neighbours p =
  [
    { x = p.x    ; y = p.y + 1 };
    { x = p.x - 1; y = p.y     };
    { x = p.x + 1; y = p.y     };
    { x = p.x    ; y = p.y - 1 };
  ]

let count ch i =
  let rec loopy c x = function
    | 0 -> c
    | y -> loopy (if i.{ x - 1, y - 1 } = ch then c + 1 else c) x (y - 1) in
  let rec loopx c = function
    | 0 -> c
    | x -> loopx (loopy c x length) (x - 1) in
  loopx 0 length

let rec loop i pos =
  if pos = finish
  then count 'O' i
  else 
    let options = neighbours pos in
    let options = List.filter (fun p ->
      p.x >= 0 && p.x < length &&
      p.y >= 0 && p.y < length &&
      match i.{ p.x, p.y } with
      | '.' -> true
      | '>' when p.x - 1 = pos.x -> true
      | '<' when p.x + 1 = pos.x -> true
      | '^' when p.y + 1 = pos.y -> true
      | 'v' when p.y - 1 = pos.y -> true
      | _ -> false
    ) options in
    List.fold_left (fun longest p ->
        let copy = Array2.create Char C_layout length length in
        let () = Array2.blit i copy in
        let () = copy.{ p.x, p.y } <- 'O' in
        max longest (loop copy p)) 0 options

let part1 = loop island start

let () = Printf.printf "part 1 = %i\n" part1
let () = flush stdout



let rec loop i pos =
(*  let () = print i in *)
  if pos = finish
  then count 'O' i
  else 
    let options = neighbours pos in
    let options = List.filter (fun p ->
      p.x >= 0 && p.x < length &&
      p.y >= 0 && p.y < length &&
      match i.{ p.x, p.y } with
      | '<' | '^' | 'v' | '>' | '.' -> true
      | _ -> false
    ) options in
    List.fold_left (fun longest p ->
        let copy = Array2.create Char C_layout length length in
        let () = Array2.blit i copy in
        let () = copy.{ p.x, p.y } <- 'O' in
        max longest (loop copy p)) 0 options

let part2 = loop island start

let () = Printf.printf "part 2 = %i\n" part2
let () = flush stdout
