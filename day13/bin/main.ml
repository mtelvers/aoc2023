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

module Valley = Map.Make (struct
  type t = coord
  let compare = compare
end)

let valleys, valley, _ = List.fold_left (fun (valleys, valley, location) line ->
    if line = ""
    then
      (valley :: valleys, Valley.empty, { x = 1; y = 0 })
    else
      String.fold_left (fun (valleys, valley, location) ch ->
        valleys, (Valley.add location ch valley), { x = location.x + 1; y = location.y }
      ) (valleys, valley, { x = 1; y = location.y + 1 }) line
  ) ([], Valley.empty, { x = 1; y = 0 }) (List.rev read_input)

let valleys = valley :: valleys

let () = Printf.printf "Number of valleys %i\n" (List.length valleys)

let vector comp_fun valley =
  Valley.fold (fun k _ lst ->
    let seen = List.fold_left (fun seen vec ->
      let any, _ = Valley.choose vec in
      seen || comp_fun any k) false lst in
    if seen then lst
    else Valley.filter (fun m _ -> comp_fun k m) valley :: lst) valley []

let rows valley =
  vector (fun p1 p2 -> p1.y = p2.y) valley

let columns valley =
  vector (fun p1 p2 -> p1.x = p2.x) valley

let valleys = List.map (fun v ->
    List.fold_left (fun acc row ->
        let _, cl = Valley.bindings row |> List.split in
        String.concat "" (List.map (String.make 1) cl) :: acc
        ) [] (rows v),
    List.fold_left (fun acc col ->
        let _, cl = Valley.bindings col |> List.split in
        String.concat "" (List.map (String.make 1) cl) :: acc
        ) [] (columns v)
    ) valleys

let rec find_pair seen = function
  | [] -> (seen, [])
  | a :: b :: tl -> if a = b then (seen @ [a], b :: tl) else find_pair (seen @ [a]) (b :: tl)
  | a :: [] -> (seen @ [a], [])

let rec list_equal l1 l2 =
  match l1, l2 with
  | [], [] | _, [] | [], _ -> true
  | hd1 :: tl1, hd2 :: tl2 -> if hd1 = hd2 then list_equal tl1 tl2 else false

let sum = List.fold_left (fun sum (r, c) ->
    let () = List.iter (Printf.printf "%s\n") r in
    let () = Printf.printf "\n" in
    let () = List.iter (Printf.printf "%s\n") c in
    let () = Printf.printf "\n" in
    let rec sum_pairs sum m l1 l2 =
      let p1, p2 = find_pair l1 l2 in
      if List.length p2 > 0 then
        let () = List.iter (Printf.printf "p1 %s\n") p1 in
        let () = List.iter (Printf.printf "p2 %s\n") p2 in
        let () = Printf.printf "\n" in
        let () = Printf.printf "%s\n" (if List.length p2 > 0 && list_equal (List.rev p1) p2 then "equal" else "not equal") in
        if list_equal (List.rev p1) p2
        then sum + (m * (List.length p1))
        else sum_pairs sum m p1 p2
      else
        sum in
    let sum = sum_pairs sum 100 [] r in
    let sum = sum_pairs sum 1 [] c in
    sum
  ) 0 valleys

let () = Printf.printf "part 1 %i\n" sum
