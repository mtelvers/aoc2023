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

type level = Low | High

let rec gcd a = function
   0 -> a
 | b -> gcd b (a mod b)

type aoc_module = Broadcaster | Flipflop | Conjunction

module Inputs = Map.Make (struct
    type t = string
    let compare = compare
  end)

type record = {
  t : aoc_module;
  outputs : string list;
  state : level;
  inputs : level Inputs.t;
}

let circuit = Hashtbl.create 100

let () = List.iter (fun line ->
  let split = Str.full_split (Str.regexp "[ ->,]") line |> List.filter (fun (x : Str.split_result) -> x <> Delim " ") in
  let rec csv_to_list (lst: Str.split_result list) =
    match lst with
    | [] -> []
    | Text s :: tl -> s :: csv_to_list tl
    | _ :: tl -> csv_to_list tl in
    match (split : Str.split_result list) with
    | Delim "%" :: Text name :: Delim "-" :: Delim ">" :: tl -> Hashtbl.add circuit name ({ t = Flipflop; outputs = csv_to_list tl; state = Low; inputs = Inputs.empty })
    | Delim "&" :: Text name :: Delim "-" :: Delim ">" :: tl -> Hashtbl.add circuit name ({ t = Conjunction; outputs = csv_to_list tl; state = Low; inputs = Inputs.empty })
    | Text "broadcaster" :: Delim "-" :: Delim ">" :: tl -> Hashtbl.add circuit "broadcaster" ({ t = Broadcaster; outputs = csv_to_list tl; state = Low; inputs = Inputs.empty })
    | _ -> assert false
  ) read_input

let () = Hashtbl.iter (fun name m ->
      List.iter (fun o ->
        match Hashtbl.find_opt circuit o with
        | Some mo when mo.t = Conjunction ->
          Hashtbl.replace circuit o ({ t = mo.t; outputs = mo.outputs; state = mo.state; inputs = Inputs.add name Low mo.inputs })
        | _ -> ()
      ) m.outputs
  ) circuit

let () = Hashtbl.iter (fun k v ->
    let () = match v.t with
    | Broadcaster ->
      let () = Printf.printf "Boardcaster : " in
      List.iter (Printf.printf "%s,") v.outputs
    | Flipflop ->
      let () = Printf.printf "Flip-flop %s : " k in
      List.iter (Printf.printf "%s,") v.outputs
    | Conjunction ->
      let () = Printf.printf "Conjunction %s : " k in
      List.iter (Printf.printf "%s,") v.outputs in
    Printf.printf "\n"
  ) circuit

type pulse = {
  source : string;
  destination : string;
  state : level;
}

let pulse = [ { source = "button"; destination = "broadcaster"; state = Low } ]

let process = List.fold_left (fun acc p ->
    match Hashtbl.find_opt circuit p.destination with
    | None -> acc
    | Some m ->
      match m.t with
        | Broadcaster ->
            let () = Hashtbl.replace circuit p.destination ({ t = Broadcaster; outputs = m.outputs; state = Low; inputs = Inputs.empty }) in
            acc @ List.map (fun n -> { source = "broadcaster"; destination = n; state = Low }) m.outputs
        | Conjunction ->
            let inputs = Inputs.add p.source p.state m.inputs in
            let () = Hashtbl.replace circuit p.destination ({ t = Conjunction; outputs = m.outputs; state = Low; inputs }) in
            let out = Inputs.fold (fun _ lvl acc -> lvl = High && acc) inputs true in
            acc @ List.map (fun n -> { source = p.destination; destination = n; state = if out then Low else High }) m.outputs
        | Flipflop when p.state = Low ->
            let state = if m.state = Low then High else Low in
            let () = Hashtbl.replace circuit p.destination ({ t = Flipflop; outputs = m.outputs; state; inputs = m.inputs }) in
            acc @ List.map (fun n -> { source = p.destination; destination = n; state }) m.outputs
        | _ -> acc
  ) []

let rec loop (low, high) lst =
  let lst = process lst in
  let low, high = List.fold_left (fun (l, h) p -> if p.state = Low then (l + 1, h) else (l, h + 1)) (low, high) lst in
(*
  let () = Printf.printf "---\n" in
  let () = List.iter (fun i -> Printf.printf "%s -%s-> %s\n" i.source (if i.state = High then "high" else "low") i.destination) lst in
   *)
  if List.length lst > 0 then loop (low, high) lst else (low, high)

let rec run low high n =
  match n with
  | 0 -> low, high
  | n -> let low, high = loop (low + 1, high) pulse in run low high (n - 1)

let low, high = run 0 0 1000

let () = Printf.printf "Part 1 : %i * %i = %i\n" low high (low * high)




let () = Hashtbl.filter_map_inplace (fun _ m ->
    Some ({ t = m.t; outputs = m.outputs; state = Low; inputs = Inputs.map (fun _ -> Low) m.inputs })
  ) circuit

let final_module = Hashtbl.fold (fun name m acc -> if List.mem "rx" m.outputs then name else acc) circuit ""

let num_inputs_to_final_module =
  Hashtbl.fold (fun name m acc -> if List.mem final_module m.outputs then name :: acc else acc) circuit [] |> List.length

let rec loop inputs n lst =
  let lst = process lst in
  let inputs = List.fold_left (fun inputs p ->
      if p.state = High && p.destination = final_module && not (Inputs.mem p.source inputs)
      then Inputs.add p.source n inputs
      else inputs
    ) inputs lst in
  let lst, n = if List.length lst = 0 then pulse, n + 1 else lst, n in
  if Inputs.cardinal inputs = num_inputs_to_final_module
  then inputs
  else loop inputs n lst

let cycles = loop Inputs.empty 1 pulse

let part2 = Inputs.fold (fun _ v x -> x * v / (gcd x v)) cycles 1

let () = Printf.printf "Part 2 : %i\n" part2
