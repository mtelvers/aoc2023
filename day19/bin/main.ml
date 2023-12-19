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

let accepted parts = List.filter (fun p ->
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

let part1 = List.fold_left (fun sum p -> sum + p.x + p.m + p.a + p.s) 0 (accepted parts)

let () = Printf.printf "part 1 : %i\n" part1



type part_range = {
  x : int * int;
  m : int * int;
  a : int * int;
  s : int * int;
  outcome : string;
}

let sec x =
  let _, b = x in b

let rec loop nodes =
  let finished, run = List.partition (fun p -> p.outcome = "A" || p.outcome = "R") nodes in
  if run = []
  then finished
  else loop (finished @ 
  List.fold_left (fun sum node ->
  let w = List.find (fun w -> w.name = node.outcome) workflows in
  let nodes, _ = List.fold_left (fun (lst, part) rule ->
    let r1, r2 = match rule.variable, rule.condition with
      | "x", ">" ->
        { x = rule.number + 1, (sec part.x); m = part.m; a = part.a; s = part.s; outcome = rule.target },
        { x = (fst part.x), rule.number; m = part.m; a = part.a; s = part.s; outcome = rule.target }
      | "m", ">" ->
        { m = rule.number + 1, (sec part.m); x = part.x; a = part.a; s = part.s; outcome = rule.target },
        { m = (fst part.m), rule.number; x = part.x; a = part.a; s = part.s; outcome = rule.target }
      | "a", ">" ->
        { a = rule.number + 1, (sec part.a); m = part.m; x = part.x; s = part.s; outcome = rule.target },
        { a = (fst part.a), rule.number; m = part.m; x = part.x; s = part.s; outcome = rule.target }
      | "s", ">" ->
        { s = rule.number + 1, (sec part.s); m = part.m; a = part.a; x = part.x; outcome = rule.target },
        { s = (fst part.s), rule.number; m = part.m; a = part.a; x = part.x; outcome = rule.target }
      | "x", "<" ->
        { x = (fst part.x), rule.number - 1; m = part.m; a = part.a; s = part.s; outcome = rule.target },
        { x = rule.number, (sec part.x); m = part.m; a = part.a; s = part.s; outcome = rule.target }
      | "m", "<" ->
        { m = (fst part.m), rule.number - 1; x = part.x; a = part.a; s = part.s; outcome = rule.target },
        { m = rule.number, (sec part.m); x = part.x; a = part.a; s = part.s; outcome = rule.target }
      | "a", "<" ->
        { a = (fst part.a), rule.number - 1; m = part.m; x = part.x; s = part.s; outcome = rule.target },
        { a = rule.number, (sec part.a); m = part.m; x = part.x; s = part.s; outcome = rule.target }
      | "s", "<" ->
        { s = (fst part.s), rule.number - 1; m = part.m; a = part.a; x = part.x; outcome = rule.target },
        { s = rule.number, (sec part.s); m = part.m; a = part.a; x = part.x; outcome = rule.target }
      | "", _ -> 
        { x = part.x; m = part.m; a = part.a; s = part.s; outcome = rule.target }, part
      | _ -> assert false in
    r1 :: lst, r2
    ) ([], node) w.rules
    in (nodes @ sum)
  ) [] run)

let nodes = loop [ { x = (1, 4000); m = (1, 4000); a = (1, 4000); s = (1, 4000); outcome = "in"; } ]

let part2 = List.fold_left (fun sum pr -> 
    if pr.outcome = "A"
    then sum + ((1 + (sec pr.x) - (fst pr.x)) * (1 + (sec pr.m) - (fst pr.m)) * (1 + (sec pr.a) - (fst pr.a)) * (1 + (sec pr.s) - (fst pr.s)))
    else sum
  ) 0 nodes

let () = Printf.printf "part 2 : %i\n" part2

