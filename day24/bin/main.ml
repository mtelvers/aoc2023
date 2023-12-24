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

type hailstone = {
  pos : float * float * float;
  dir : float * float * float;
}

let hailstones = List.fold_left (fun lst line ->
    (Scanf.sscanf line " %f, %f, %f @ %f, %f, %f " (fun x1 y1 z1 x2 y2 z2 -> { pos = (x1, y1, z1); dir = ( x2, y2, z2 ) })) :: lst
  ) [] read_input

let vec_to_string v =
  let x, y, z = v in
  Printf.sprintf "(%f,%f,%f)" x y z

let vec_add v1 v2 =
  let x1, y1, z1 = v1 in
  let x2, y2, z2 = v2 in
  ( x1 +. x2, y1 +. y2, z1 +. z2 )

let inverse m1 =
  let a, b, c, d = m1 in
  let det = a *. d -. b *. c in
  ( d /. det, -. b /. det, -. c /. det, a /. det )

let multiply m1 v1 =
  let a, b, c, d = m1 in
  let e, f = v1 in
  ( a *. e +. b *. f, c *. e +. d *. f )

let multiply_scalar v1 s =
  let a, b, c = v1 in
  ( a *. s, b *. s, c *. s )

let () =
  List.iter (fun v -> Printf.printf "%s %s\n" (vec_to_string v.pos) (vec_to_string v.dir)) hailstones

let solve_linear p1 d1 p2 d2 =
  let p1x, p1y, _ = p1 in
  let d1x, d1y, _ = d1 in
  let p2x, p2y, _ = p2 in
  let d2x, d2y, _ = d2 in
  let inv_m = inverse ( d2x, -. d1x, d2y, -. d1y ) in
  let a = ( p1x -. p2x, p1y -. p2y ) in
  multiply inv_m a

let rec crossing_hailstones low high count = function
  | [] -> count
  | h1 :: tl ->
    (List.fold_left (fun sum h2 ->
    let t, u = solve_linear h1.pos h1.dir h2.pos h2.dir in
    if t >= 0. && u >= 0.
    then
      let cross_x, cross_y, _ = vec_add h1.pos (multiply_scalar h1.dir u) in
      if low <= cross_x && cross_x <= high && low <= cross_y && cross_y <= high
      then
     (*   let () = Printf.printf "h1 %s h2 %s at %s\n" (vec_to_string h1.pos) (vec_to_string h2.pos) (vec_to_string (cross_x, cross_y, 0.)) in *)
        sum + 1
      else sum
    else sum
  ) 0 tl) + (crossing_hailstones low high 0 tl)

(* let part1 = crossing_hailstones 7. 27. 0 hailstones *)
let part1 = crossing_hailstones 200_000_000_000_000. 400_000_000_000_000. 0 hailstones

let () = Printf.printf "part 1 - %i\n" part1
