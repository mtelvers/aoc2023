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

let platform = List.rev read_input

let flip x = List.init (String.length (List.hd x)) (fun i -> List.map (fun s -> String.make 1 (String.get s i)) x |> String.concat String.empty)

let print p =
  let () = List.iter (Printf.printf "%s\n") p in
  Printf.printf "\n\n"

let tip ?(rev = false) p =
  let tip_string s =
    List.map (fun s ->
      let nrocks = String.fold_left (fun acc ch -> if ch = 'O' then acc + 1 else acc) 0 s in
      match rev with
      | false -> String.init nrocks (fun _ -> 'O') ^ String.init ((String.length s) - nrocks) (fun _ -> '.')
      | true -> String.init ((String.length s) - nrocks) (fun _ -> '.') ^ String.init nrocks (fun _ -> 'O')
    ) (String.split_on_char '#' s) |> String.concat "#" in
  List.map (tip_string) p

let () = print (flip (tip (flip platform)))

let load p =
  List.mapi (fun i s -> (i + 1) * String.fold_left (fun acc ch -> if ch = 'O' then acc + 1 else acc) 0 s) (List.rev p) |> List.fold_left ( + ) 0

let () = Printf.printf "part 1 %i\n" (load (flip (tip (flip platform))))


let cache = Hashtbl.create 1_000
let results = Hashtbl.create 1_000

let rec loop n p =
  match Hashtbl.find_opt cache p with
    | Some x -> (x, n)
    | None ->
      let () = Hashtbl.add results n (load p) in
      let () = Hashtbl.add cache p n in
      let north = tip (flip p) in
      let west = tip (flip north) in
      let south = tip ~rev:true (flip west) in
      let east = tip ~rev:true (flip south) in
      loop (n + 1) east

let entry, last = loop 0 platform

let final = entry + ((1_000_000_000 - entry) mod (last - entry))
let () = Printf.printf "part 2 %i\n" (Hashtbl.find results final)
