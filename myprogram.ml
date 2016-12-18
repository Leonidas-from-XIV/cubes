type cube = Cube of int

exception Too_big

let extract1 (x, y, z) (Cube n) =
  if n > x || n > y || n > z then
    (* Trying to pull out a Cube of size n of something that is of less than n? *)
    raise Too_big
  else
    (* Calculate the count and sizes of the respective split cuboids *)
    match (n = x, n = y, n = z) with
    | true, true, true -> None
    | true, true, false -> Some [(x, y, z-n)]
    | true, false, true -> Some [(x, y-n, z)]
    | false, true, true -> Some [(x-n, y, z)]
    | false, false, true -> Some [(x-n, y, z); (n, y-n, z)]
    | false, true, false -> Some [(n, y, z-n); (x-n, y, z)]
    | true, false, false -> Some [(x, n, z-n); (x, y-n, z)]
    | false, false, false -> Some [(n, n, z-n); (x-n, n, z); (x, y-n, z)]

exception Impossibru

let rec extract geom = function
  (* We ran out of cubes to extract from the geometry *)
  | [] -> raise Impossibru
  (* Try to extract the first cube *)
  | cube::cubes -> match extract1 geom cube with
    | exception Too_big ->
        (* That cube was too big, try again with the next *)
        let (matches, cubes) = extract geom cubes in
        (matches, cube::cubes)
    | None -> (1, cubes)
    (* Then we just recurse on the different ways the cuboids could be split *)
    | Some geoms ->
        let (matches, cubes) =
          List.fold_left (fun (matches, cubes) geom ->
            let (matches', cubes') = extract geom cubes in
            (matches + matches', cubes')) (0, cubes) geoms
        in
        (matches + 1, cubes)

let solve ((x, y, z), cubes) =
  match extract (x, y, z) cubes with
  | (i, _) -> i
  | exception Impossibru -> -1

let parse line =
  (* no \d? okay.jpg *)
  let r = Str.regexp "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)\\(.*\\)" in
  (* wtf what a horrible API *)
  let _ = Str.string_match r line 0 in
  let x = int_of_string @@ Str.matched_group 1 line in
  let y = int_of_string @@ Str.matched_group 2 line in
  let z = int_of_string @@ Str.matched_group 3 line in
  (* Convert all these numbers into n times Cube (position ** 2) *)
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
