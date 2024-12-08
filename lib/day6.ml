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

let equal_paths path1 path2 =
  List.for_alli path1 ~f:(fun idx pos1 ->
    let pos2 = List.nth_exn path2 idx in
    Vec.equal pos1 pos2)
;;

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
      None
    end
    else begin
      match Set.mem obstacles (next_x, next_y) with
      | true -> Some { guard with direction = Direction.rotate_right guard.direction }
      | false -> Some { guard with position = next_x, next_y }
    end
  ;;

  exception LoopDetected

  (** Check for loop in a given [guard]'s path, given [obstacles] and map [bounds].
      Runs simulation for [max_iters] iterations. When we have over 8 'edges' of the path,
      begins checking whether the last 4 are identical to past 4, i.e [a,b,d,c,a,b,c,d]. *)
  let check_for_loop guard obstacles bounds ~max_iters =
    let rec aux g acc iter =
      match step g obstacles bounds with
      | _ when iter > max_iters -> raise LoopDetected
      | Some next_guard ->
        (* we should have 8 positions already, start checking for loops*)
        if iter > 9
        then begin
          let latest_four = List.slice acc 0 4 in
          let prev_four = List.slice acc 5 9 in
          if equal_paths latest_four prev_four then raise LoopDetected
        end;

        aux next_guard (next_guard.position :: acc) (iter + 1)
      | None -> acc
    in
    aux guard [] 0
  ;;
end

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
    let rec aux g acc =
      match Guard.step g obstacles bounds with
      | Some next_guard -> aux next_guard (next_guard.position :: acc)
      | None -> acc
    in

    let result = aux guard [ guard.position ] |> PosSet.of_list |> Set.length in
    print_endline @@ string_of_int result
  ;;

  (* Run part 2 with parsed inputs *)
  let part2 ({ guard; obstacles; bounds } : t) =
    let rec aux g acc =
      match Guard.step g obstacles bounds with
      | Some next_guard -> aux next_guard (next_guard.position :: acc)
      | None -> acc
    in

    (* all positions where guard originally went through *)
    let original_path = aux guard [ guard.position ] |> PosSet.of_list |> Set.to_list in

    let count_valid_obstacles original_path guard obstacles bounds ~domain_mgr =
      (* Split work across all cores *)
      let num_workers = Domain.recommended_domain_count () in
      let chunk_size = List.length original_path / num_workers in
      let chunks = List.chunks_of original_path ~length:chunk_size in

      let counter = Atomic.make 0 in

      let fibers =
        List.mapi chunks ~f:(fun _chunk_index chunk ->
          (* Create a fiber/thunk for each chunk that... *)
          let thunk () =
            (* Defines the logic, process the chunk and track result in atomic counter *)
            let process () =
              List.iteri chunk ~f:(fun _idx position ->
                let new_obstacles = Set.add obstacles position in
                try ignore @@ Guard.check_for_loop guard new_obstacles bounds ~max_iters:7000 with
                | Guard.LoopDetected -> Atomic.incr counter)
            in
            (* Submits the processing logic to domain mgr to execute in one of the cores *)
            Eio.Domain_manager.run domain_mgr process
          in
          thunk)
      in
      Eio.Fiber.all fibers;

      Atomic.get counter
    in

    (* Run an EIO loop for multi-core threading *)
    Eio_main.run
    @@ fun env ->
    let valid_count =
      count_valid_obstacles
        ~domain_mgr:(Eio.Stdenv.domain_mgr env)
        original_path
        guard
        obstacles
        bounds
    in
    print_endline @@ string_of_int valid_count
  ;;
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
let%expect_test "part 1" =
  run example ~only_part1:true;
  [%expect {| 41 |}]
;;

let%expect_test "part 2" =
  run example ~only_part2:true;
  [%expect {| 6 |}]
;;
