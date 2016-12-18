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
        let item = Cube (int_of_float ((float_of_int i) ** 2.0)) in
        let rec loop acc = function
          | 0 -> acc
          | n -> loop (item::acc) (n-1)
        in loop [] n)
    |> List.flatten
    |> List.rev
  in
  ((x, y, z), cubes)

exception Too_big

let cut (x, y, z) (Cube n) =
  if n > x || n > y || n > z then
    raise Too_big
  else
    match (n = x, n = y, n = z) with
    | true, true, true -> []
    | true, true, false -> [(x, y, z - n)]
    | true, false, true -> [(x, y-n, z)]
    | false, true, true -> [(x-n, y, z)]
    | false, false, true -> [(x-n, y, z); (n, y-n, z)]
    | false, true, false -> [(n, y, z-n); (x-n, y, z)]
    | true, false, false -> [(x, n, z-n); (x, y-n, z)]
    | false, false, false -> [(n, n, z-n); (x-n, n, z); (x, y-n, z)]

let solve ((x, y, z), cubes) =
  (-1)

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
