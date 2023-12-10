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

type compass = North | South | East | West

type paper = {
  ch : char;
  d : int;
}

module Field = Map.Make (struct
  type t = coord
  let compare = compare
end)

let field, _ = List.fold_left (fun (field, location) line ->
    String.fold_left (fun (field, location) ch ->
      (Field.add location { ch; d = 0 } field), { x = location.x + 1; y = location.y }
    ) (field, { x = 1; y = location.y + 1 }) line
  ) (Field.empty, { x = 1; y = 0 }) (List.rev read_input)


let neighbours p =
  [
    { x = p.x    ; y = p.y + 1 }, North;
    { x = p.x - 1; y = p.y     }, East;
    { x = p.x + 1; y = p.y     }, West;
    { x = p.x    ; y = p.y - 1 }, South;
  ]

let print f =
  let _ = Field.fold (fun k v y ->
    let () = if k.y != y then Printf.printf "\n" in
    let () = Printf.printf "%c(%i)" v.ch v.d in
    k.y) f 1 in
  Printf.printf "\n\n"

let _ = print field

let rec loop s f d =
(*  let _ = print f in *)
  List.fold_left (fun f (pos, access) ->
      match Field.find_opt pos f with
      | None -> f
      | Some v ->
        match v.ch with
        | '7' ->
          if (access = West || access = South) && v.d = 0
          then loop pos (Field.add pos { ch = v.ch; d } f) (d + 1)
          else f
        | '-' ->
          if (access = West || access = East) && v.d = 0
          then loop pos (Field.add pos { ch = v.ch; d } f) (d + 1)
          else f
        | 'F' ->
          if (access = South || access = East) && v.d = 0
          then loop pos (Field.add pos { ch = v.ch; d } f) (d + 1)
          else f
        | 'J' ->
          if (access = North || access = West) && v.d = 0
          then loop pos (Field.add pos { ch = v.ch; d } f) (d + 1)
          else f
        | '|' ->
          if (access = North || access = South) && v.d = 0
          then loop pos (Field.add pos { ch = v.ch; d } f) (d + 1)
          else f
        | 'L' ->
          if (access = North || access = East) && v.d = 0
          then loop pos (Field.add pos { ch = v.ch; d } f) (d + 1)
          else f
        | _ -> f
    ) f (neighbours s)

let start, _ = Field.filter (fun _ v -> v.ch = 'S') field |> Field.choose

let field = loop start field 1

let max = Field.fold (fun _ v a -> if v.d > a then v.d else a) field 0

let () = Printf.printf "part 1: %i\n" ((max + 1) / 2)

