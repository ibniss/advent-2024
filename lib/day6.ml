open Core

module Direction = struct
  type t =
    | North
    | East
    | West
    | South
  [@@deriving show]

  let rotate_right = function
    | North -> East
    | East -> South
    | South -> West
    | West -> North
  ;;

  let to_symbol = function
    | North -> '^'
    | East -> '>'
    | South -> 'v'
    | West -> '<'
  ;;

  (** Convert direction to a 1-length vector. Note: assuming north is -1 since we count columns top to bottom *)
  let to_vec = function
    | North -> 0, -1
    | East -> 1, 0
    | South -> 0, 1
    | West -> -1, 0
  ;;

  let of_symbol = function
    | '^' -> North
    | '>' -> East
    | 'v' -> South
    | '<' -> West
    | x -> failwith (Printf.sprintf "Invalid direction symbol %c" x)
  ;;
end

module Vec = struct
  type t = int * int [@@deriving compare, sexp, show]

  let make ~x ~y : t = x, y
  let equal (a, b) (a', b') = a = a' && b = b'
  let add (a, b) (a', b') = a + a', b + b'
end

module PosSet = Set.Make (Vec)

module Guard = struct
  type t =
    { position : Vec.t
    ; direction : Direction.t
    }
  [@@deriving show]

  (** Perform a [guard] step, with present [obstacles] in a map of size [max_x, max_y]*)
  let step guard obstacles (max_x, max_y) =
    let next_x, next_y = Vec.add guard.position (Direction.to_vec guard.direction) in

    (* out of bounds *)
    if next_x > max_x || next_x < 0 || next_y > max_y || next_y < 0
    then begin
      print_endline "Guard escaped!";
      None
    end
    else begin
      match Set.mem obstacles (next_x, next_y) with
      | true -> begin
        Printf.printf
          "Rotate to %c\n"
          (guard.direction |> Direction.rotate_right |> Direction.to_symbol);
        Some { guard with direction = Direction.rotate_right guard.direction }
      end
      | false -> begin
        Printf.printf "Walk to (%d, %d)\n" next_x next_y;
        Some { guard with position = next_x, next_y }
      end
    end
  ;;
end

(* keep map of (x,y) -> obstacle; guard: (x,y), directioni, function to walk forward, function to
   check obstacle and walk; walk and keep track of (x,y) pairs; then unique it*)
module M = struct
  (* Type to parse the input into *)
  type t =
    { obstacles : PosSet.t
    ; guard : Guard.t
    ; bounds : Vec.t
    }

  type parse_interim =
    { obstacles : PosSet.t
    ; guard : Guard.t option
    }

  let print_obstacles (obstacles : PosSet.t) =
    Set.iter obstacles ~f:(fun o -> printf "%s\n" (Vec.show o))
  ;;

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs : t =
    let split_lines = String.split_lines inputs in
    let max_y = List.length split_lines - 1 in
    let max_x = String.length (List.nth_exn split_lines 0) - 1 in
    match
      split_lines
      |> List.foldi ~init:{ obstacles = PosSet.empty; guard = None } ~f:(fun col acc line ->
        print_endline line;
        let { obstacles = line_obstacles; guard = line_guard } =
          String.to_list line
          |> List.foldi ~init:{ obstacles = PosSet.empty; guard = None } ~f:(fun row acc' ch ->
            let position = Vec.make ~x:row ~y:col in
            match ch with
            | '#' -> { acc' with obstacles = Set.add acc'.obstacles position }
            | ('^' | '>' | '<' | 'v') as dir ->
              { acc' with guard = Some { position; direction = Direction.of_symbol dir } }
            | _ -> acc')
        in
        match line_guard with
        | Some g when Option.is_none acc.guard ->
          { guard = Some g; obstacles = Set.union acc.obstacles line_obstacles }
        | Some _ -> failwith "Duplicate guard found?"
        | None -> { acc with obstacles = Set.union acc.obstacles line_obstacles })
    with
    | { guard = None; _ } -> failwith "Failed to find guard position"
    | { guard = Some g; obstacles } -> { guard = g; obstacles; bounds = Vec.make ~x:max_x ~y:max_y }
  ;;

  (* Run part 1 with parsed inputs *)
  let part1 ({ guard; obstacles; bounds } : t) =
    print_endline @@ Guard.show guard;
    print_obstacles obstacles;

    let rec aux g acc =
      match Guard.step g obstacles bounds with
      | Some next_guard -> aux next_guard (next_guard.position :: acc)
      | None -> acc
    in

    let result = aux guard [ guard.position ] |> PosSet.of_list |> Set.length in
    print_endline @@ string_of_int result
  ;;

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "....#.....\n\
   .........#\n\
   ..........\n\
   ..#.......\n\
   .......#..\n\
   ..........\n\
   .#..^.....\n\
   ........#.\n\
   #.........\n\
   ......#..."
;;

(* Expect test for example input *)
let%expect_test _ =
  run example ~only_part1:true;
  [%expect {| 41 |}]
;;
