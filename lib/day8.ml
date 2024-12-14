open Core
open Utils

module Antenna = struct
  type t =
    { pos : Vec.t
    ; freq : char
    }
  [@@deriving compare, sexp, show]

  let equal a b = Char.equal a.freq b.freq && Vec.equal a.pos b.pos
end

module M = struct
  open Antenna

  (* Type to parse the input into *)
  type t =
    { antennas : Antenna.t list
    ; bounds : Vec.t
    }
  [@@deriving compare, sexp, show]

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    (* parse into a dynarray since we don't care much about parsing perf and its simple *)
    let antennas : Antenna.t Dynarray.t = Dynarray.create () in

    let input_lines = String.split_lines inputs in

    input_lines
    |> List.iteri ~f:(fun col line ->
      List.iteri (String.to_list line) ~f:(fun row ch ->
        match ch with
        | '.' -> ()
        | other when Char.is_alpha other || Char.is_digit other -> begin
          Dynarray.add_last antennas { pos = Vec.make ~x:row ~y:col; freq = other }
        end
        | _ -> ()));

    { antennas = Dynarray.to_list antennas
    ; bounds =
        Vec.make ~x:(String.length (List.nth_exn input_lines 0) - 1) ~y:(List.length input_lines - 1)
    }
  ;;

  let in_bounds (pos : Vec.t) (bounds : Vec.t) =
    pos.x >= 0 && pos.x <= bounds.x && pos.y >= 0 && pos.y <= bounds.y
  ;;

  let find_antinodes a b bounds =
    let diff = Vec.sub b.pos a.pos in

    let rec aux_add latest acc =
      let next = Vec.add diff latest in
      if in_bounds next bounds then aux_add next (next :: acc) else acc
    in

    let rec aux_sub latest acc =
      let next = Vec.sub latest diff in
      if in_bounds next bounds then aux_sub next (next :: acc) else acc
    in

    (* first find all nodes forward *)
    let forward_nodes = aux_add b.pos [] in
    (* then add backward nodes to them *)
    aux_sub a.pos forward_nodes
  ;;

  (* Run part 1 with parsed inputs *)
  let part1 { antennas; bounds } =
    let positions = Dynarray.create () in

    (* 1. group by antenna freq *)
    let antennas_by_freq =
      List.sort_and_group antennas ~compare:(fun a b -> Char.compare a.freq b.freq)
    in

    (* 2. for each freq, pairwise consider the antennas and add a new antinode location to the
       list *)
    List.iter antennas_by_freq ~f:(fun group ->
      List.cartesian_product group group
      |> List.filter ~f:(fun (a, b) -> not (Vec.equal a.pos b.pos))
      |> List.iter ~f:(fun (a, b) ->
        (* difference vector of positions *)
        let diff = Vec.sub b.pos a.pos in

        (* compute positions on either side *)
        let pos1 = Vec.add b.pos diff in
        let pos2 = Vec.sub a.pos diff in

        if in_bounds pos1 bounds
        then begin
          Dynarray.add_last positions pos1
        end;

        if in_bounds pos2 bounds
        then begin
          Dynarray.add_last positions pos2
        end));

    (* filter out unique values*)
    positions
    |> Dynarray.to_list
    |> List.stable_dedup ~compare:Vec.compare
    |> List.length
    |> string_of_int
    |> print_endline
  ;;

  (* Run part 2 with parsed inputs *)
  (* same as above, but repeat add/sub until we run out of board, add all valid*)
  let part2 { antennas; bounds } =
    let positions = Dynarray.create () in

    (* 1. group by antenna freq *)
    let antennas_by_freq =
      List.sort_and_group antennas ~compare:(fun a b -> Char.compare a.freq b.freq)
    in

    (* 2. for each freq, pairwise consider the antennas and add a new antinode location to the
       list *)
    List.iter antennas_by_freq ~f:(fun group ->
      List.cartesian_product group group
      |> List.filter ~f:(fun (a, b) -> not (Vec.equal a.pos b.pos))
      |> List.iter ~f:(fun (a, b) ->
        let antinodes = find_antinodes a b bounds in
        Dynarray.append_list positions antinodes));

    (* add positions of each antenna since they also act as antinodes *)
    Dynarray.append_list positions (antennas |> List.map ~f:(fun a -> a.pos));

    (* filter out unique values*)
    positions
    |> Dynarray.to_list
    |> List.stable_dedup ~compare:Vec.compare
    |> List.length
    |> string_of_int
    |> print_endline
  ;;
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "............\n\
   ........0...\n\
   .....0......\n\
   .......0....\n\
   ....0.......\n\
   ......A.....\n\
   ............\n\
   ............\n\
   ........A...\n\
   .........A..\n\
   ............\n\
   ............"
;;

let example_small =
  "T....#....\n\
   ...T......\n\
   .T....#...\n\
   .........#\n\
   ..#.......\n\
   ..........\n\
   ...#......\n\
   ..........\n\
   ....#.....\n\
   .........."
;;

(* Expect test for example input *)
let%expect_test "part 1" =
  run example ~only_part1:true;
  [%expect {| 14 |}]
;;

let%expect_test "part 2 small" =
  run example_small ~only_part2:true;
  [%expect {| 9 |}]
;;

let%expect_test "part 2 original" =
  run example ~only_part2:true;
  [%expect {| 34 |}]
;;
