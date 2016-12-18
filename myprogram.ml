type cube = Cube of int

let parse line =
  let r = Str.regexp "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)\\(.*\\)" in
  (* wtf what a horrible API *)
  let _ = Str.string_match r line 0 in
  let x = int_of_string @@ Str.matched_group 1 line in
  let y = int_of_string @@ Str.matched_group 2 line in
  let z = int_of_string @@ Str.matched_group 3 line in
  let cubes =
    Str.matched_group 4 line
    |> Str.split (Str.regexp " ")
    |> List.map int_of_string
    |> List.mapi (fun i n ->
        let item = Cube (int_of_float (2.0 ** (float_of_int i))) in
        let rec loop acc = function
          | 0 -> acc
          | n -> loop (item::acc) (n-1)
        in loop [] n)
    |> List.flatten
    |> List.rev
  in
  ((x, y, z), cubes)

exception Too_big

type 'a subdivisions =
  | Zero
  | One of ('a * 'a * 'a)
  | Two of ('a * 'a * 'a) * ('a * 'a * 'a)
  | Three of ('a * 'a * 'a) * ('a * 'a * 'a) * ('a * 'a * 'a)

let extract (x, y, z) (Cube n) =
  if n > x || n > y || n > z then
    raise Too_big
  else
    match (n = x, n = y, n = z) with
    | true, true, true -> Zero
    | true, true, false -> One (x, y, z - n)
    | true, false, true -> One (x, y-n, z)
    | false, true, true -> One (x-n, y, z)
    | false, false, true -> Two ((x-n, y, z), (n, y-n, z))
    | false, true, false -> Two ((n, y, z-n), (x-n, y, z))
    | true, false, false -> Two ((x, n, z-n), (x, y-n, z))
    | false, false, false -> Three ((n, n, z-n), (x-n, n, z), (x, y-n, z))

exception Impossibru

let rec helper geom cubes =
  match cubes with
  | [] -> raise Impossibru
  | cube::cubes -> match extract geom cube with
    | exception Too_big -> helper geom cubes
    | Zero -> (1, cubes)
    | One geom ->
        let (matches, cubes) = helper geom cubes in
        (matches + 1, cubes)
    | Two (geom1, geom2) ->
        let (matches1, cubes) = helper geom1 cubes in
        let (matches2, cubes) = helper geom2 cubes in
        (matches1 + matches2 + 1, cubes)
    | Three (geom1, geom2, geom3) ->
        let (matches1, cubes) = helper geom1 cubes in
        let (matches2, cubes) = helper geom2 cubes in
        let (matches3, cubes) = helper geom3 cubes in
        (matches1 + matches2 + matches3 + 1, cubes)

let solve ((x, y, z), cubes) =
  match helper (x, y, z) cubes with
  | (i, _) -> i
  | exception Impossibru -> -1

let process line =
  line
  |> parse
  |> solve
  |> string_of_int
  |> print_endline

let main () =
  let rec loop () =
    match read_line () with
    | line -> process line; loop ()
    | exception End_of_file -> ()
  in
  loop ()

let () =
  main ()
