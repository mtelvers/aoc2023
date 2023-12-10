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

type paper = {
  ch : char;
  d : int;
  real : bool;
}

module Field = Map.Make (struct
  type t = coord
  let compare = compare
end)

let field, _ = List.fold_left (fun (field, location) line ->
    String.fold_left (fun (field, location) ch ->
      (Field.add location { ch; d = 0; real = true } field), { x = location.x + 1; y = location.y }
    ) (field, { x = 1; y = location.y + 1 }) line
  ) (Field.empty, { x = 1; y = 0 }) (List.rev read_input)

let neighbours p =
  [
    { x = p.x    ; y = p.y + 1 };
    { x = p.x - 1; y = p.y     };
    { x = p.x + 1; y = p.y     };
    { x = p.x    ; y = p.y - 1 };
  ]

let reachable_neighbours p ch =
  match ch with
  | '-' -> [ { x = p.x - 1; y = p.y }; { x = p.x + 1; y = p.y } ]
  | '|' -> [ { x = p.x; y = p.y + 1 }; { x = p.x; y = p.y - 1 } ]
  | 'L' -> [ { x = p.x; y = p.y - 1 }; { x = p.x + 1; y = p.y } ]
  | 'J' -> [ { x = p.x; y = p.y - 1 }; { x = p.x - 1; y = p.y } ]
  | '7' -> [ { x = p.x - 1; y = p.y }; { x = p.x; y = p.y + 1 } ]
  | 'F' -> [ { x = p.x + 1; y = p.y }; { x = p.x; y = p.y + 1 } ]
  | 'S' -> [ { x = p.x; y = p.y + 1 }; { x = p.x - 1; y = p.y }; { x = p.x + 1; y = p.y }; { x = p.x; y = p.y - 1 } ]
  | _ -> []

let print ?(value = true) f =
  let _ = Field.fold (fun k v y ->
    let () = if k.y != y then Printf.printf "\n" in
    let () = match value with
      | true -> if v.d > 0 then Printf.printf "*" else Printf.printf "%c" v.ch
      | false -> Printf.printf "%c" v.ch in
    k.y) f 1 in
  Printf.printf "\n\n"

let rec loop s ch f d =
  (*
  let _ = Printf.printf "HERE\n" in
  let _ = print f in
     *)
  List.fold_left (fun f pos ->
    match Field.find_opt pos f with
    | None -> f
    | Some v ->
      let reciprocal = List.mem s (reachable_neighbours pos v.ch) in
      if v.d = 0 && reciprocal
      then loop pos v.ch (Field.add pos { ch = v.ch; d; real = v.real } f) (d + 1)
      else f
    ) f (reachable_neighbours s ch)

let start, _ = Field.filter (fun _ v -> v.ch = 'S') field |> Field.choose

let field = loop start 'S' (Field.add start { ch = 'S'; d = 1; real = true } field) 2

let max = Field.fold (fun _ v a -> if v.d > a then v.d else a) field 0

let () = Printf.printf "part 1: %i\n" (max / 2)


let field = Field.map (fun v ->
  if v.d = 0
  then { ch = '.'; d = 0; real = true }
  else { ch = '+'; d = v.d; real = true } ) field

let field = Field.fold (fun k v a ->
  let extras p = [ { x = 2 * p.x + 1; y = 2 * p.y }; { x = 2 * p.x; y = 2 * p.y + 1 }; { x = 2 * p.x + 1; y = 2 * p.y + 1 } ] in
  List.fold_left (fun acc v -> Field.add v { ch = ' '; d = 0; real = false } acc )
    (Field.add { x = k.x * 2; y = k.y * 2 } { ch = v.ch; d = v.d * 2; real = true } a) (extras k)) field Field.empty

let field = Field.mapi (fun k v ->
  List.fold_left (fun acc (p1, p2) ->
    let v1 = Field.find_opt p1 field in
    let v2 = Field.find_opt p2 field in
    match v1, v2 with
      | Some v1, Some v2 -> let delta = abs (v1.d - v2.d) in
        if v1.real && v2.real && v1.d > 0 && v2.d > 0 && ((delta = 2) || (delta = (2 * max - 2)))
        then { ch = '+' ; d = (v1.d + v2.d) / 2; real = false } else acc
      | _ -> acc
    ) v [ { x = k.x - 1; y = k.y }, { x = k.x + 1; y = k.y }; { x = k.x; y = k.y - 1 }, { x = k.x; y = k.y + 1 }; ]) field

let rec flood f o =
  let v = Field.find o f in
  List.fold_left (fun acc p ->
    try
      let v = Field.find p acc in
      match v.ch with
      | ' ' | '.' -> flood acc p
      | _ -> acc
    with Not_found -> acc) (Field.add o { ch = '#'; d = v.d; real = v.real } f) (neighbours o)

let field =
  let min, _ = Field.min_binding field in
  let max, _ = Field.min_binding field in
  let edges = Field.filter (fun k v ->
    v.ch = '.' && (k.x = min.x || k.y = min.y || k.x = max.x - 1 || k.y = max.y - 1)) field in
  Field.fold (fun k _ a -> flood a k) edges field

let sum = Field.fold (fun _ v sum -> if v.ch = '.' then sum + 1 else sum) field 0

let () = Printf.printf "part 2: %i\n" sum
