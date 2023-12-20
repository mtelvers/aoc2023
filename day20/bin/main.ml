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

let invert = function
  | Low -> High
  | High -> Low

type aoc_module = Button | Broadcaster | Flipflop | Conjunction

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

(*
module Circuit = Map.Make (struct
  type t = string
  let compare = compare
end)
   *)

let circuit = Hashtbl.create 100

(*
let circuit = List.fold_left (fun circuit line ->
  let split = Str.full_split (Str.regexp "[ ->,]") line |> List.filter (fun (x : Str.split_result) -> x <> Delim " ") in
  let rec csv_to_list (lst: Str.split_result list) =
    match lst with
    | [] -> []
    | Text s :: tl -> s :: csv_to_list tl
    | _ :: tl -> csv_to_list tl in
    match (split : Str.split_result list) with
    | Delim "%" :: Text name :: Delim "-" :: Delim ">" :: tl -> Hashtbl.add name (Flipflop { outputs = csv_to_list tl; state = Low }) circuit
    | Delim "&" :: Text name :: Delim "-" :: Delim ">" :: tl -> Hashtbl.add name (Conjunction { outputs = csv_to_list tl; state = Low }) circuit
    | Text "broadcaster" :: Delim "-" :: Delim ">" :: tl -> Hashtbl.add "broadcaster" (Broadcaster { outputs = csv_to_list tl; state = Low }) circuit
    | _ -> assert false
  ) ((Hashtbl.add "button" (Button { outputs = ["broadcaster"]; state = Low })) Hashtbl.empty) read_input
    *)

let () = Hashtbl.add circuit "button" ({ t = Button; outputs = ["broadcaster"]; state = Low; inputs = Inputs.empty })

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

let () = Hashtbl.iter (fun k v ->
    let () = match v.t with
    | Button ->
      let () = Printf.printf "Button : " in
      List.iter (Printf.printf "%s,") v.outputs
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

(*
    let b = Hashtbl.find circuit "button"

let inputs = match b with
        | Button r -> List.map (fun n -> { name = n; state = Low }) r.outputs

let () = Printf.printf "---\n"
let () = List.iter (fun i -> Printf.printf "%s %s\n" i.name (if i.state = High then "high" else "low")) inputs

    let m = List.map (fun i -> Hashtbl.find circuit i.name) inputs

let inputs = List.fold_left (fun acc b ->
    match b with
        | Button r -> acc @ List.map (fun n -> { name = n; state = Low }) r.outputs
        | Broadcaster r -> acc @ List.map (fun n -> { name = n; state = Low }) r.outputs
  ) [] m

let () = Printf.printf "---\n"
let () = List.iter (fun i -> Printf.printf "%s %s\n" i.name (if i.state = High then "high" else "low")) inputs

*)

let pulses = [ { source = "button"; destination = "broadcaster"; state = Low } ]


let process = List.fold_left (fun acc p ->
    let m = Hashtbl.find circuit p.destination in
    match m.t with
        | Broadcaster ->
            let () = Hashtbl.add circuit p.destination ({ t = Broadcaster; outputs = m.outputs; state = Low; inputs = Inputs.empty }) in
            acc @ List.map (fun n -> { source = "broadcaster"; destination = n; state = Low }) m.outputs
        | Conjunction ->
            let inputs = Inputs.add p.source p.state m.inputs in
            let () = Hashtbl.add circuit p.destination ({ t = Conjunction; outputs = m.outputs; state = Low; inputs }) in
            let out = Inputs.fold (fun k lvl acc -> lvl = High && acc) inputs true in
            acc @ List.map (fun n -> { source = p.destination; destination = n; state = if out then Low else High }) m.outputs
        | Flipflop when p.state = Low ->
            let state = if m.state = Low then High else Low in
            let () = Hashtbl.add circuit p.destination ({ t = Flipflop; outputs = m.outputs; state; inputs = m.inputs }) in
            acc @ List.map (fun n -> { source = p.destination; destination = n; state }) m.outputs
        | _ -> acc
  ) []

let rec loop lst =
  let lst = process lst in
  let () = Printf.printf "---\n" in
  let () = List.iter (fun i -> Printf.printf "%s -%s-> %s\n" i.source (if i.state = High then "high" else "low") i.destination) lst in
  if List.length lst > 0 then loop lst else ()

let () = loop pulses


