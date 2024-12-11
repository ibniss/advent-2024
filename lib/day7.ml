open Core

module Equation = struct
  let try_match expected inputs =
    let rec aux = function
      | a :: b :: rest ->
        let mult_result = a * b in
        let add_result = a + b in

        aux (mult_result :: rest) || aux (add_result :: rest)
      | [ result ] -> result = expected
      | [] -> failwith "Should not reduce to empty list?"
    in
    aux inputs
  ;;
end

module M = struct
  (* Type to parse the input into *)
  type t = calibration list

  and calibration =
    { result : int
    ; sequence : int list
    }
  [@@deriving show]

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    inputs
    |> String.split_lines
    |> List.map ~f:(fun line ->
      let parts = String.split line ~on:':' in
      let result = List.nth_exn parts 0 |> int_of_string in
      let nums = List.nth_exn parts 1 in
      let numbers = nums |> String.strip |> String.split ~on:' ' |> List.map ~f:int_of_string in
      { result; sequence = numbers })
  ;;

  (* Run part 1 with parsed inputs *)
  let part1 inputs =
    inputs
    |> List.filter ~f:(fun { result; sequence } -> Equation.try_match result sequence)
    |> List.map ~f:(fun { result; _ } -> result)
    |> List.fold ~init:0 ~f:( + )
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
  "190: 10 19\n\
   3267: 81 40 27\n\
   83: 17 5\n\
   156: 15 6\n\
   7290: 6 8 6 15\n\
   161011: 16 10 13\n\
   192: 17 8 14\n\
   21037: 9 7 18 13\n\
   292: 11 6 16 20"
;;

(* Expect test for example input *)
let%expect_test _ =
  run example;
  [%expect {| 3749 |}]
;;
