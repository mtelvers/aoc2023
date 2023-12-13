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

module String = struct
  include String
  let from_char_list cl =
    String.concat "" (List.map (String.make 1) cl)
end

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

let valley_from_rows lines =
  let valley, _ = List.fold_left (fun (valley, location) line ->
    String.fold_left (fun (valley, location) ch ->
      (Valley.add location ch valley), { x = location.x + 1; y = location.y }
    ) (valley, { x = 1; y = location.y + 1 }) line
  ) (Valley.empty, { x = 1; y = 0 }) lines in
  valley

let valley_from_columns lines =
  let valley, _ = List.fold_left (fun (valley, location) line ->
    String.fold_left (fun (valley, location) ch ->
      (Valley.add location ch valley), { x = location.x; y = location.y + 1 }
    ) (valley, { x = location.x + 1; y = 1 }) line
  ) (Valley.empty, { x = 0; y = 1 }) lines in
  valley

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

let rec find_pair seen = function
  | [] -> (seen, [])
  | a :: b :: tl -> if a = b then (seen @ [a], b :: tl) else find_pair (seen @ [a]) (b :: tl)
  | a :: [] -> (seen @ [a], [])

let rec list_equal l1 l2 =
  match l1, l2 with
  | [], [] | _, [] | [], _ -> true
  | hd1 :: tl1, hd2 :: tl2 -> if hd1 = hd2 then list_equal tl1 tl2 else false

let count_differences s1 s2 =
  let l1 = List.init (String.length s1) (String.get s1) in
  let l2 = List.init (String.length s2) (String.get s2) in
  List.fold_left2 (fun count c1 c2 -> if c1 = c2 then count else count + 1) 0 l1 l2

let rec print r c =
  match r, c with
  | [], [] -> ()
  | hd1 :: tl1, [] -> let () = Printf.printf "%40s\n" hd1 in print tl1 []
  | [], hd2 :: tl2 -> let () = Printf.printf "                                        %40s\n" hd2 in print [] tl2
  | hd1 :: tl1, hd2 :: tl2 -> let () = Printf.printf "%40s%40s\n" hd1 hd2 in print tl1 tl2

let rec score valley recurse =
    let r = List.fold_left (fun acc row ->
      let _, cl = Valley.bindings row |> List.split in
      (String.from_char_list cl) :: acc) [] (rows valley) in
    let c = List.fold_left (fun acc col ->
      let _, cl = Valley.bindings col |> List.split in
      (String.from_char_list cl) :: acc) [] (columns valley) in
    let () = print r c in
    let rec sum_pairs sum m l1 l2 =
      let p1, p2 = find_pair l1 l2 in
      if List.is_empty p2
      then sum
      else
        let () = List.iter (Printf.printf "p1 %s\n") p1 in
        let () = List.iter (Printf.printf "p2 %s\n") p2 in
        let () = Printf.printf "\n" in
        let () = Printf.printf "%s\n" (if List.length p2 > 0 && list_equal (List.rev p1) p2 then "equal" else "not equal") in
        if list_equal (List.rev p1) p2
        then sum_pairs ((m * (List.length p1)) :: sum) m p1 p2
        else sum_pairs sum m p1 p2 in
    let sum = sum_pairs [] 100 [] r in
    let sum = sum_pairs sum 1 [] c in
        let () = Printf.printf "%sscores:" (if not recurse then "" else "secondary ") in
        let () = List.iter (Printf.printf "%i,") sum in
        let () = Printf.printf "\n" in
    let rec all_differences alts row_or_column lst = function
      | [] -> alts
      | hd :: tl ->
        let rec check nv lst2 = function
          | [] -> nv
          | hd2 :: tl2 ->
            if count_differences hd hd2 = 1
            then check ((lst @ [hd] @ lst2 @ [hd] @ tl2) :: (lst @ [hd2] @ lst2 @ [hd2] @ tl2) :: nv) (lst2 @ [hd2]) tl2
            else check nv (lst2 @ [hd2]) tl2 in
        let new_possibilities = check [] [] tl in
        let new_valleys = List.map (fun x -> if row_or_column then valley_from_rows x else valley_from_columns x) new_possibilities in
        let () = Printf.printf "alternatives: %i\n" (List.length new_valleys) in
        let alternate_scores = List.map (fun v -> score v false) new_valleys |> List.flatten in
        let () = Printf.printf "end alternatives\n" in
        let () = Printf.printf "alternate scores:" in
        let () = List.iter (Printf.printf "%i,") alternate_scores in
        let () = Printf.printf "\n" in
      all_differences (alternate_scores @ alts) row_or_column (lst @ [hd]) tl in
    match recurse with
    | true ->
      let alts = all_differences [] true [] r in
      let alts = all_differences alts false [] c in
      let alts = List.filter (fun x -> x > 0 && not (List.mem x sum)) alts in
      let () = List.iter (Printf.printf "NEW SCORE %i\n") alts in
      List.sort_uniq (compare) alts
    | false -> sum

let sum = List.fold_left (fun sum valley -> sum + List.fold_left ( + ) 0 (score valley false)) 0 valleys

let () = Printf.printf "part 1 %i\n" sum


let sum = List.fold_left (fun sum valley ->
                                            let s = List.fold_left ( + ) 0 (score valley true) in
                                            let () = Printf.printf "XXX %i\n" s in
                         sum + s) 0 valleys

let () = Printf.printf "part 2 %i\n" sum

