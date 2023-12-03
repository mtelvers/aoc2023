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
  let of_char_opt c =
    try Some (int_of_string (String.init 1 (fun _ -> c)))
    with Failure _ -> None
end

type coord = {
  y : int;
  x : int;
}

type proximity = Near of coord | Far

type paper = {
  ch : char;
  p : proximity;
}

module Schematic = Map.Make (struct
  type t = coord
  let compare = compare
end)

let schematic, _ = List.fold_left (fun (schematic, location) line ->
    String.fold_left (fun (schematic, location) ch ->
      (Schematic.add location { ch; p = Far } schematic), { x = location.x + 1; y = location.y }
    ) (schematic, { x = 1; y = location.y + 1 }) line
  ) (Schematic.empty, { x = 1; y = 0 }) (List.rev read_input)

let symbols = [ '*'; '#'; '-'; '+'; '$'; '/'; '='; '@'; '&'; '%' ]

let neighbours p =
  [
    { x = p.x - 1; y = p.y + 1 };
    { x = p.x    ; y = p.y + 1 };
    { x = p.x + 1; y = p.y + 1 };
    { x = p.x - 1; y = p.y     };
    { x = p.x + 1; y = p.y     };
    { x = p.x - 1; y = p.y - 1 };
    { x = p.x    ; y = p.y - 1 };
    { x = p.x + 1; y = p.y - 1 };
  ]

let schematic =
  let sym = Schematic.filter (fun _ v -> List.mem v.ch symbols) schematic in
  Schematic.fold (fun origin _ schematic ->
    List.fold_left (fun schematic pos ->
      let rec update schematic pos =
        match Schematic.find_opt pos schematic with
        | Some place ->
          (match place.ch, place.p with
          | '0', Far | '1', Far | '2', Far | '3', Far | '4', Far | '5', Far | '6', Far | '7', Far | '8', Far | '9', Far ->
            let ns = Schematic.add pos { ch = place.ch; p = Near origin } schematic in
            let ns = update ns { x = pos.x + 1; y = pos.y } in
            update ns { x = pos.x - 1; y = pos.y }
          | _ -> schematic)
        | None -> schematic in
      update schematic pos
    ) schematic (neighbours origin)
) sym schematic

let current, sum =
  Schematic.fold (fun _ v (current, sum) ->
    match Int.of_char_opt v.ch, v.p with
    | Some n, Near _ -> (current * 10 + n, sum)
    | _ -> (0, sum + current)
  ) schematic (0, 0)
let sum = sum + current

let () = Printf.printf "part 1: %i\n" sum

let _, _, gears =
  Schematic.fold (fun _ v (current, position, gears) ->
    match Int.of_char_opt v.ch, v.p with
    | Some n, Near place ->
      (current * 10 + n, Some place, gears)
    | _ ->
      match position with
      | Some p -> let sym = Schematic.find p schematic in
        (match sym.ch, Schematic.find_opt p gears with
        | '*', Some r -> (0, None, Schematic.add p (current :: r) gears)
        | '*', None -> (0, None, Schematic.add p [ current ] gears)
        | _ -> (0, None, gears))
      | None -> (0, None, gears)
  ) schematic (0, None, Schematic.empty)

let sum = Schematic.fold (fun _ lst sum ->
    if List.length lst > 1
    then sum + List.fold_left (fun ls x -> ls * x) 1 lst
    else sum
  ) gears 0

let () = Printf.printf "part 2: %i\n" sum

