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

let valleys, valley = List.fold_left (fun (valleys, valley) line ->
    if line = ""
    then (valley :: valleys, [])
    else (valleys, valley @ [ line ])
  ) ([], []) (List.rev read_input)

let valleys = valley :: valleys

let () = Printf.printf "Number of valleys %i\n" (List.length valleys)

let flip x = List.init (String.length (List.hd x)) (fun i -> List.map (fun s -> String.make 1 (String.get s i)) x |> String.concat "")

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

let rec score valley recurse =
    let flipped_valley = flip valley in
    let rec sum_pairs sum m l1 l2 =
      let p1, p2 = find_pair l1 l2 in
      if List.is_empty p2
      then sum
      else
        if list_equal (List.rev p1) p2
        then sum_pairs ((m * (List.length p1)) :: sum) m p1 p2
        else sum_pairs sum m p1 p2 in
    let sum = sum_pairs [] 100 [] valley in
    let sum = sum_pairs sum 1 [] flipped_valley in
    let rec all_differences alts flipped lst = function
      | [] -> alts
      | hd :: tl ->
        let rec check nv lst2 = function
          | [] -> nv
          | hd2 :: tl2 ->
            if count_differences hd hd2 = 1
            then check ((lst @ [hd] @ lst2 @ [hd] @ tl2) :: (lst @ [hd2] @ lst2 @ [hd2] @ tl2) :: nv) (lst2 @ [hd2]) tl2
            else check nv (lst2 @ [hd2]) tl2 in
        let new_possibilities = check [] [] tl in
        let new_valleys = List.map (fun x -> if flipped then flip x else x) new_possibilities in
        let alternate_scores = List.map (fun v -> score v false) new_valleys |> List.flatten in
      all_differences (alternate_scores @ alts) flipped (lst @ [hd]) tl in
    match recurse with
    | true ->
      let alts = all_differences [] false [] valley in
      let alts = all_differences alts true [] flipped_valley in
      let alts = List.filter (fun x -> x > 0 && not (List.mem x sum)) alts in
      List.sort_uniq (compare) alts
    | false -> sum

let sum = List.fold_left (fun sum valley -> sum + List.fold_left ( + ) 0 (score valley false)) 0 valleys

let () = Printf.printf "part 1 %i\n" sum


let sum = List.fold_left (fun sum valley -> sum + List.fold_left ( + ) 0 (score valley true)) 0 valleys

let () = Printf.printf "part 2 %i\n" sum

