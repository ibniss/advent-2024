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

type antenna_pair = Antenna.t * Antenna.t

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

  (* Run part 1 with parsed inputs *)
  let part1 { antennas; bounds } =
    let positions = Dynarray.create () in

    (* 1. group by antenna freq *)
    let antennas_by_freq =
      List.sort_and_group antennas ~compare:(fun a b -> Char.compare a.freq b.freq)
    in

    (*List.iter antennas_by_freq ~f:(fun group ->*)
    (*  print_endline "Group:";*)
    (*  List.iter group ~f:(fun ant -> print_endline @@ Antenna.show ant));*)

    (* 2. for each freq, pairwise consider the antennas and add a new antinode location to the
       list *)
    List.iter antennas_by_freq ~f:(fun group ->
      List.cartesian_product group group
      |> List.filter ~f:(fun (a, b) -> not (Vec.equal a.pos b.pos))
      |> List.iter ~f:(fun (a, b) ->
        (*print_endline "Considering antennas:";*)
        (*print_endline @@ Antenna.show a;*)
        (*print_endline @@ Antenna.show b;*)

        (* difference vector of positions *)
        let diff = Vec.sub b.pos a.pos in
        (*Printf.printf "Diff: %s\n" (Vec.show diff);*)
        (* compute positions on either side *)
        let pos1 = Vec.add b.pos diff in
        let pos2 = Vec.sub a.pos diff in

        (*Printf.printf "Pos1: %s\n" (Vec.show pos1);*)
        (*Printf.printf "Pos2: %s\n" (Vec.show pos2);*)
        if in_bounds pos1 bounds
        then begin
          (*print_endline "1 valid";*)
          Dynarray.add_last positions pos1
        end;

        if in_bounds pos2 bounds
        then begin
          (*print_endline "2 valid";*)
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
  let part2 _ = ()
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

(* Expect test for example input *)
let%expect_test "part 1" =
  run example ~only_part1:true;
  [%expect {| 14 |}]
;;
