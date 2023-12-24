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
        sum + 1
      else sum
    else sum
  ) 0 tl) + (crossing_hailstones low high 0 tl)

(* let part1 = crossing_hailstones 7. 27. 0 hailstones *)
let part1 = crossing_hailstones 200_000_000_000_000. 400_000_000_000_000. 0 hailstones

let () = Printf.printf "part 1 - %i\n" part1


let () =
  let oc = open_out "solve.ae" in
  let () = Printf.fprintf oc "logic px, py, pz, dx, dy, dz : real\n" in
  let () = List.iteri (fun i h ->
    let px, py, pz = h.pos in
    let dx, dy, dz = h.dir in
    let () = Printf.fprintf oc "logic t%03d : real\n" i in
    let () = Printf.fprintf oc "axiom cx%03d : %.1f + (%.1f * t%03d) = px + (dx * t%03d)\n" i px dx i i in
    let () = Printf.fprintf oc "axiom cy%03d : %.1f + (%.1f * t%03d) = py + (dy * t%03d)\n" i py dy i i in
    let () = Printf.fprintf oc "axiom cz%03d : %.1f + (%.1f * t%03d) = pz + (dz * t%03d)\n" i pz dz i i in
    let () = Printf.fprintf oc "check_sat g%03d : t%03d > 0.0\n" i i in
    ()
  ) hailstones in
  close_out oc

(*
 * opam install alt-ergo
 * Then `alt-ergo solve.ae --dump-models`
 *

File "solve.ae", line 46, characters 19-29: I don't know (0.8420) (349 steps) (goal at008)
(
  (define-fun px () Real 287430900705823)
  (define-fun py () Real 451620998712421)
  (define-fun pz () Real 260730677041648)
  (define-fun dx () Real (- 20))
  (define-fun dy () Real (- 274))
  (define-fun dz () Real 31)
  (define-fun t000 () Real 654071052858)
  (define-fun t001 () Real 857208450422)
  (define-fun t002 () Real 556101734365)
  (define-fun t003 () Real 476479664657)
  (define-fun t004 () Real 168465616583)
  (define-fun t005 () Real 727041923887)
  (define-fun t006 () Real 620070537368)
  (define-fun t007 () Real 616838084871)
  (define-fun t008 () Real 578700563495)
  (define-fun t009 () Real 417370510294)
)

Sum px, py and pz for the answer

   *)
