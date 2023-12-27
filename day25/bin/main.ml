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

let nodes = List.fold_left (fun all line ->
  let split = String.split_on_char ':' line in
  let n0 = List.hd split in
  let nn = List.nth split 1 in
  let lst = String.split_on_char ' ' nn |> List.filter (fun s -> String.length s > 0) in
  (n0, lst) :: all) [] read_input

(* fdp -Tpdf graph.gv > graph.pdf *)
let () =
  let oc = open_out "graph.gv" in
  let () = Printf.fprintf oc "digraph g1 {\n" in
  let () = Printf.fprintf oc "  layout=\"fdp\";\n" in
  let () = Printf.fprintf oc "  overlay=\"scale\";\n" in
  let () = Printf.fprintf oc "  edge [ arrowhead=\"none\" ];\n" in
  let () = List.iter (fun (n0, nl) ->
    List.iter (fun n -> Printf.fprintf oc "  %s -> %s;\n" n0 n) nl) nodes in
  let () = Printf.fprintf oc "}\n" in
  close_out oc

let edges =
  List.fold_left (fun lst (n0, nl) ->
      List.fold_left (fun lst n -> (n0, n) :: lst) lst nl
    ) [] nodes

let nodes =
  let l1, l2 = List.split edges in
  List.sort_uniq (compare) (l1 @ l2)

let () = Printf.printf "# of nodes %i\n" (List.length nodes)

(*
let rec dfs_route best path tn =
  let n0 = List.hd path in
  if n0 = tn
  then path
  else
    List.filter_map (fun (e0, e1) ->
        if n0 = e0 || n0 = e0 then Some e1 else
        if n0 = e1 || n0 = e1 then Some e0 else
          None ) edges |>
    List.filter (fun n -> not (List.mem n path)) |>
    List.fold_left (fun b n ->
        let nr = dfs_route b (n :: path) tn in
        match (List.length nr), (List.length b) with
        | _, 0 -> nr
        | 0, _ -> b
        | lnr, lb -> if lnr < lb then nr else b
      ) best

let print route =
    let () = List.iter (fun n -> Printf.printf "%s -> " n) (List.rev route) in
    let () = Printf.printf "\n" in
    flush stdout
 *)

let rec bfs_route paths tn =
  let shortest = List.fold_left (fun shortest path ->
      if List.hd path = tn
      then
        match (List.length path), (List.length shortest) with
        | _, 0 -> path
        | lp, ls -> if lp < ls then path else shortest
      else shortest) [] paths in
  if List.length shortest > 0
  then shortest
  else
    let visited_nodes = List.flatten paths |> List.sort_uniq (compare) in
    let new_paths = List.fold_left (fun new_paths path ->
        let n1 = List.hd path in
        (List.filter_map (fun (e0, e1) ->
            if n1 = e0 || n1 = e0 then Some e1 else
            if n1 = e1 || n1 = e1 then Some e0 else
              None) edges |>
        List.filter (fun n -> not (List.mem n visited_nodes)) |>
        List.map (fun n0 -> n0 :: path)) @ new_paths
      ) [] paths in
      bfs_route new_paths tn

let rec route_to_edge = function
    | [] -> []
    | _ :: [] -> []
    | n0 :: n1 :: tl -> (if compare n0 n1 <= 0 then n0, n1 else n1, n0) :: route_to_edge (n1 :: tl)

let heatmap = Hashtbl.create 1000

let () = for _ = 0 to 100 do
    let n0 = List.nth nodes (Random.int (List.length nodes)) in
    let n1 = List.nth nodes (Random.int (List.length nodes)) in
    let r = bfs_route [[n0]] n1 in
    let e = route_to_edge r in
    List.iter (fun n ->
        match Hashtbl.find_opt heatmap n with
        | None -> Hashtbl.add heatmap n 1
        | Some x -> Hashtbl.replace heatmap n (x + 1)) e
  done

let ordered_heatmap = Hashtbl.to_seq heatmap |> List.of_seq |> List.sort (fun (_, i0) (_, i1) -> compare i1 i0)

let rec count_nodes edges nodes =
  let reachable_nodes = List.map (fun n0 ->
        List.filter_map (fun (e0, e1) ->
            if n0 = e0 || n0 = e0 then Some e1 else
            if n0 = e1 || n0 = e1 then Some e0 else
              None) edges
      ) nodes |> List.flatten |> List.sort_uniq (compare) in
  let new_nodes = List.filter (fun n -> not (List.mem n nodes)) reachable_nodes in
  if List.length new_nodes = 0
  then List.length nodes
  else count_nodes edges (nodes @ new_nodes)

let remove_edge (r0, r1) =
  List.filter (fun (n0, n1) -> not (n0 = r0 && n1 = r1 || n1 = r0 && n0 = r1))

let new_edges = List.fold_left (fun acc i ->
    let n, _ = List.nth ordered_heatmap i in
    remove_edge n acc
  ) edges [0; 1; 2]

let count1 = count_nodes new_edges ["cpq"]
let count2 = count_nodes new_edges ["hlx"]
let () = Printf.printf "%i nodes, %i nodes, part 1 %i\n" count1 count2 (count1 * count2)

