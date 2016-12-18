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
  in
  ((x, y, z), cubes)

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
