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

let tup4 re = Tyre.conv
  (fun (((x, y), z), w) -> ((x, y, z), w))
  (fun ((x, y, z), w) -> ((x, y), z), w)
  re

let extract_values =
  let open Tyre in
  pos_int <&> (str " " *> pos_int) <&> (str " " *> pos_int) <&> (list @@ str " " *> pos_int)
  |> tup4
  |> compile
  |> exec

let parse line =
  let open CCResult.Infix in
  extract_values line >|= fun (coords, cubes_per_power) ->
    let cubes = cubes_per_power
    |> List.mapi (fun i n ->
        let rec loop acc = function
          | 0 -> acc
          | n -> loop (Cube (1 lsl i)::acc) (n-1)
        in loop [] n)
    |> List.flatten
    |> List.rev
    in
    (coords, cubes)

let process line =
  let open CCResult.Infix in
  let r = line
  |> parse
  >|= solve
  >|= string_of_int
  in match r with
  | Ok v -> print_endline v
  | Error _ -> print_endline "-1"

let rec main () =
  match read_line () with
  | line -> process line; main ()
  | exception End_of_file -> ()

let () =
  main ()
