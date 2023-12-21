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

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

type coord = {
  y : int;
  x : int;
}

module Farm = Map.Make (struct
  type t = coord
  let compare = compare
end)

let farm, _ = List.fold_left (fun (farm, location) line ->
    String.fold_left (fun (farm, location) ch ->
      (Farm.add location ch farm), { x = location.x + 1; y = location.y }
    ) (farm, { x = 1; y = location.y + 1 }) line
  ) (Farm.empty, { x = 1; y = 0 }) (List.rev read_input)

let neighbours p =
  [
    { x = p.x    ; y = p.y + 1 };
    { x = p.x - 1; y = p.y     };
    { x = p.x + 1; y = p.y     };
    { x = p.x    ; y = p.y - 1 };
  ]

let print ?(reachable=[]) f =
  let _ = Farm.fold (fun k v y ->
    let () = if k.y != y then Printf.printf "\n" in
    let () = Printf.printf "%c" (if List.mem k reachable then 'O' else v) in
    k.y) f 1 in
  Printf.printf "\n\n"

(*
let () = print farm
    *)

let start = Farm.fold (fun k v start -> if v = 'S' then k else start) farm { x = 1; y = 1 }

let rec expand = function
  | [] -> []
  | hd :: tl -> (List.filter (fun p ->
      match Farm.find_opt p farm with
      | Some 'S'
      | Some '.' -> true
      | _ -> false
      ) (neighbours hd)) @ expand tl

let rec loop lst = function
  | 0 -> lst
  | n -> loop (List.sort_uniq (compare) (expand lst)) (n - 1)

let l = loop [start] 64

(*
let () = List.iter (fun p -> Printf.printf "(%i,%i)" p.x p.y) l
let () = Printf.printf "\n"
*)

let () = Printf.printf "part 1 = %i\n" (List.length l)

(*
 3 * 11 + 5 = 38
 4 * 11 + 5 = 49
 5 * 11 + 5 = 60

38: 894
  0  0  0  0  0  0  0  0  0
  0  0  0  7 14  7  0  0  0
  0  0  7 33 42 33  7  0  0
  0  7 33 42 39 42 33  7  0
  0 14 42 39 42 39 41 11  0
  0  7 33 42 39 42 24  1  0
  0  0  7 33 41 24  1  0  0
  0  0  0  7 11  1  0  0  0
  0  0  0  0  0  0  0  0  0

49: 1528
  0  0  0  7 14  7  0  0  0
  0  0  7 33 42 33  7  0  0
  0  7 33 42 39 42 33  7  0
  7 33 42 39 42 39 42 33  7
 14 42 39 42 39 42 39 41 11
  7 33 42 39 42 39 42 24  1
  0  7 33 42 39 42 24  1  0
  0  0  7 33 41 24  1  0  0
  0  0  0  7 11  1  0  0  0

60: 2323
  0  0  0  0  7 14  7  0  0  0  0
  0  0  0  7 33 42 33  7  0  0  0
  0  0  7 33 42 39 42 33  7  0  0
  0  7 33 42 39 42 39 42 33  7  0
  7 33 42 39 42 39 41 39 42 33  7
 14 42 39 42 39 42 39 42 39 41 11
  7 33 42 39 42 39 42 39 42 24  1
  0  7 33 42 39 42 39 42 24  1  0
  0  0  7 33 42 39 42 24  1  0  0
  0  0  0  7 33 41 24  1  0  0  0
  0  0  0  0  7 11  1  0  0  0  0

 3 * 131 + 65 = 458
 4 * 131 + 65 = 589
 5 * 131 + 65 = 720
 . * 131 + 65 = ...
 202300 * 131 + 65 = 26501365

458: 177838
    0    0    0    0    0    0    0    0    0    0    0
    0    0    0    0    0    0    0    0    0    0    0
    0    0    0    0  934 5447  900    0    0    0    0
    0    0    0  934 6352 7257 6321  900    0    0    0
    0    0  934 6352 7257 7226 7257 6321  900    0    0
    0    0 5486 7257 7226 7257 7226 7257 5422    0    0
    0    0  916 6360 7257 7226 7257 6327  945    0    0
    0    0    0  916 6360 7257 6327  945    0    0    0
    0    0    0    0  916 5461  945    0    0    0    0
    0    0    0    0    0    0    0    0    0    0    0
    0    0    0    0    0    0    0    0    0    0    0

589: 293822
    0    0    0    0    0    0    0    0    0    0    0
    0    0    0    0  934 5447  900    0    0    0    0
    0    0    0  934 6352 7257 6321  900    0    0    0
    0    0  934 6352 7257 7226 7257 6321  900    0    0
    0  934 6352 7257 7226 7257 7226 7257 6321  900    0
    0 5486 7257 7226 7257 7226 7257 7226 7257 5422    0
    0  916 6360 7257 7226 7257 7226 7257 6327  945    0
    0    0  916 6360 7257 7226 7257 6327  945    0    0
    0    0    0  916 6360 7257 6327  945    0    0    0
    0    0    0    0  916 5461  945    0    0    0    0
    0    0    0    0    0    0    0    0    0    0    0

720: 438772
    0    0    0    0  934 5447  900    0    0    0    0
    0    0    0  934 6352 7257 6321  900    0    0    0
    0    0  934 6352 7257 7226 7257 6321  900    0    0
    0  934 6352 7257 7226 7257 7226 7257 6321  900    0
  934 6352 7257 7226 7257 7226 7257 7226 7257 6321  900
 5486 7257 7226 7257 7226 7257 7226 7257 7226 7257 5422
  916 6360 7257 7226 7257 7226 7257 7226 7257 6327  945
    0  916 6360 7257 7226 7257 7226 7257 6327  945    0
    0    0  916 6360 7257 7226 7257 6327  945    0    0
    0    0    0  916 6360 7257 6327  945    0    0    0
    0    0    0    0  916 5461  945    0    0    0    0


even n:
    0    0    0    0    0    0    0    0    0    0    0
    0    0    0    0  TL1  TOP  TR1    0    0    0    0
    0    0    0  TL1  TL2  ODD  TR2  TR1    0    0    0
    0    0  TL1  TL2  ODD EVEN  ODD  TR2  TR1    0    0
    0  TL1  TL2  ODD EVEN  ODD EVEN  ODD  TR2  TR1    0
    0 LEFT  ODD EVEN  ODD EVEN  ODD EVEN  ODD RIGH    0
    0  BL1  BL2  ODD EVEN  ODD EVEN  ODD  BR2  BR1    0
    0    0  BL1  BL2  ODD EVEN  ODD  BR2  BR1    0    0
    0    0    0  BL1  BL2  ODD  BR2  BR1    0    0    0
    0    0    0    0  BL1  BOT  BR1    0    0    0    0
    0    0    0    0    0    0    0    0    0    0    0

   EVEN = 7226
    ODD = 7257
    TL1 = 934
    TL2 = 6352
    TR1 = 900
    TR2 = 6321
    BL1 = 916
    BL2 = 6360
    BR1 = 945
    BR2 = 6327
    TOP = 5447
    BOT = 5461
   LEFT = 5486
   RIGH = 5422

*)
(*
let rec test_loop l n =
  if n > 720 then ()
  else
    let length = 131 in
    let l = loop l 1 in
    let () = Printf.printf "%i: %i\n" n (List.length l) in
    (* let () = print farm ~reachable:l in *)
    let () = for y = 0 to 10 do
    for x = 0 to 10 do
      let tl = { x = 1 + (x * length); y = 1 + (y * length); } in
      let br = { x = 1 + length + (x * length); y = 1 + length + (y * length); } in
        Printf.printf "%5i" (List.filter (fun p -> p.x >= tl.x && p.x < br.x && p.y >= tl.y && p.y < br.y) l |> List.length)
      done ;
        Printf.printf "\n"
    ; done
      in
      let () = flush stdout in
    test_loop l (n + 1)

let () = test_loop [start] 1
*)

let tl, _ = Farm.min_binding farm
let br, _ = Farm.max_binding farm

let steps = 26501365

let length = int_of_float (Float.sqrt (float_of_int (Farm.cardinal farm)))

let odd = loop [start] (length * 2) |> List.length
let even = loop [start] (length * 2 + 1) |> List.length
let () = Printf.printf "length %i, odd = %i, even = %i\n" length odd even

let fill = length - 1
let top = loop [{ x = start.x; y = br.y }] fill |> List.length
let bot = loop [{ x = start.x; y = tl.y }] fill |> List.length
let left = loop [{ x = br.x; y = start.y }] fill |> List.length
let righ = loop [{ x = tl.x; y = start.y }] fill |> List.length
let () = Printf.printf "%i %i %i %i\n" top bot left righ

let fill = length / 2 - 1
let tl1 = loop [{ x = br.x; y = br.y }] fill |> List.length
let tr1 = loop [{ x = tl.x; y = br.y }] fill |> List.length
let bl1 = loop [{ x = br.x; y = tl.y }] fill |> List.length
let br1 = loop [{ x = tl.x; y = tl.y }] fill |> List.length
let () = Printf.printf "%i %i %i %i\n" tl1 tr1 bl1 br1

let fill = 3 * length / 2 - 1
let tl2 = loop [{ x = br.x; y = br.y }] fill |> List.length
let tr2 = loop [{ x = tl.x; y = br.y }] fill |> List.length
let bl2 = loop [{ x = br.x; y = tl.y }] fill |> List.length
let br2 = loop [{ x = tl.x; y = tl.y }] fill |> List.length
let () = Printf.printf "%i %i %i %i\n" tl2 tr2 bl2 br2

let grid_length = steps / length - 1
let num_even_blocks = pow (grid_length / 2 * 2 + 1) 2
let num_odd_blocks = pow ((grid_length + 1) / 2 * 2) 2
let () = Printf.printf "%i %i %i\n" grid_length num_even_blocks num_odd_blocks

let () = Printf.printf "%i\n" (odd * num_odd_blocks + even * num_even_blocks +
                               top + bot + left + righ +
                               (grid_length + 1) * (tl1 + tr1 + bl1 + br1) +
                               grid_length * (tl2 + tr2 + bl2 + br2))
