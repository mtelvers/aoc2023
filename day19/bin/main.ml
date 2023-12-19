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

module Maps = Map.Make (struct
  type t = string
  let compare = compare
end)

type conditional = {
  variable : string;
  condition : string;
  number : int;
  target : string;
}

type workflow = {
  name : string;
  rules : conditional list;
}

type part = {
  x : int;
  m : int;
  a : int;
  s : int;
}

let workflows, parts = List.fold_left (fun (workflows, parts) line ->
  let split = Str.full_split (Str.regexp "[{}=:<>,]") line in
  let rec loop ws ps lst =
    match (lst : Str.split_result list) with
    | [] -> ws, ps
    | Text s1 :: Delim "{" :: tl ->
      loop ({ name = s1; rules = [] } :: ws) ps tl
    | Text s1 :: Delim d :: Text s2 :: Delim ":" :: Text s3 :: tl ->
      let w = List.hd ws in
      loop ({ name = w.name; rules = w.rules @ [{ variable = s1; condition = d; number = int_of_string s2; target = s3 }] } :: (List.tl ws)) ps tl
    | Text s1 :: Delim "}" :: tl ->
      let w = List.hd ws in
      loop ({ name = w.name; rules = w.rules @ [{ variable = ""; condition = ""; number = 0; target = s1 }] } :: (List.tl ws)) ps tl
    | Delim "{" ::
      Text "x" :: Delim "=" :: Text x :: Delim "," ::
      Text "m" :: Delim "=" :: Text m :: Delim "," ::
      Text "a" :: Delim "=" :: Text a :: Delim "," ::
      Text "s" :: Delim "=" :: Text s :: Delim "}" :: tl ->
      loop ws ({ x = int_of_string x; m = int_of_string m; a = int_of_string a; s = int_of_string s } :: ps) tl
    | _ :: tl -> loop ws ps tl in
  loop workflows parts split
  ) ([], []) read_input

let () = List.iter (fun w ->
    let () = Printf.printf "%s : " w.name in
    let () = List.iter (fun r -> Printf.printf "%s%s%i:%s," r.variable r.condition r.number r.target) w.rules in
    let () = Printf.printf "\n" in
    ()
  ) workflows

let () = List.iter (fun p ->
    let () = Printf.printf "%i %i %i %i\n" p.x p.m p.a p.s in
    () ) parts

let accepted_parts = List.filter (fun p ->
    let rec process name =
      match name with
      | "A" -> "A"
      | "R" -> "R"
      | _ ->
        let w = List.find (fun w -> w.name = name) workflows in
        List.fold_left (fun target rule ->
          match target with
          | "" ->
            (let variable = match rule.variable with
            | "x" -> p.x
            | "m" -> p.m
            | "a" -> p.a
            | "s" -> p.s
            | "" -> 0
            | _ -> assert false in
            match rule.condition with
            | "" -> process rule.target
            | ">" -> if variable > rule.number then process rule.target else ""
            | "<" -> if variable < rule.number then process rule.target else ""
            | _ -> assert false)
          | _ ->
            target
        ) "" w.rules in
      "A" = process "in"
    ) parts

let part1 = List.fold_left (fun sum p -> sum + p.x + p.m + p.a + p.s) 0 accepted_parts

let () = Printf.printf "part 1 : %i\n" part1
