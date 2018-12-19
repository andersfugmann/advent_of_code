open Core

let parse_line l =
  try Scanf.sscanf l "x=%d, y=%d..%d" (fun x y1 y2 -> x,x,y1,y2)
  with | _ -> Scanf.sscanf l "y=%d, x=%d..%d" (fun y x1 x2 -> x1,x2,y,y)

(* Calculate min and max values *)
let make_world clay =
  let open Int in
  let min_x, max_x, min_y, max_y =
    List.reduce ~f:(fun (x1, x2, y1, y2) (x1', x2', y1', y2') ->
        min x1 x1', max x2 x2', min y1 y1', max y2 y2'
      ) clay
    |> (fun v -> Option.value_exn v)
  in
  (* Add one to either side *)
  let min_x = min_x - 1 in
  let max_x = max_x + 1 in

  (* Create a 2d world map *)
  let world =
    Array.make_matrix ~dimx:(max_x - min_x + 1) ~dimy:(max_y - min_y + 1) '.'
  in

  (* Fill the world *)
  List.iter clay ~f:(fun (x1, x2, y1, y2) ->
      for x = x1 to x2 do
        for y = y1 to y2 do
          world.(x - min_x).(y - min_y) <- '#'
        done;
      done
    );
  min_x, min_y, world

let print_world (min_x, min_y, world) =
  printf "min_x, min_y: %d, %d\n" min_x min_y;
  for x = 0 to Array.length world - 1 do
    printf "%c" (match x = 500 - min_x with
        | true -> '+'
        | false -> '.');
  done;
  printf "\n";

  for y = 0 to Array.length world.(0) - 1 do
    for x = 0 to Array.length world - 1 do
      printf "%c" world.(x).(y)
    done;
    printf "\n"
  done

let rec flow world (x,y) =
  match world.(x).(y) = '.' with
  | true ->
    world.(x).(y) <- '*';
    y = Array.length world.(0) - 1 || (* At the bottom *)
    flow world (x, y + 1) || (* Free flow downward *)
    ( let left  = flow world (x-1, y) in (* Flow left *)
      let right = flow world (x+1, y) in (* Flow right *)
      left || right )
  | false -> false

(** Count where water has been *)
let count_drops world =
  Array.fold ~init:0
    ~f:(fun acc a -> Array.count ~f:(fun x -> x = '*') a + acc) world

let () =
  let (min_x, min_y, world) =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:parse_line
    |> make_world
  in
  print_world (min_x, min_y, world);
  let (_res : bool) = flow world (500 - min_x, 0) in
  print_world (min_x, min_y, world);
  printf "Result: %d\n" (count_drops world);
  ()
