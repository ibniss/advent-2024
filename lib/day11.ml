open Core

module Stone = struct
  (** strip leading zeros from [x] *)
  let strip_zeros x =
    let rec aux = function
      | '0' :: tl -> aux tl
      | hd :: tl -> hd :: tl
      | [] -> [ '0' ]
    in
    x |> String.to_list |> aux |> String.of_list
  ;;

  let split_num x =
    let str_x = string_of_int x in
    let len = String.length str_x in
    let first_part = String.slice str_x 0 (len / 2) in
    let second_part = String.slice str_x (len / 2) 0 in
    let second_trimmed = second_part |> strip_zeros in
    [ int_of_string first_part; int_of_string second_trimmed ]
  ;;

  let num_digits n =
    if n = 0 then 1 else Float.log10 (Float.of_int (abs n)) |> Float.to_int |> ( + ) 1
  ;;

  let evolve x =
    if x = 0
    then [ 1 ]
    else begin
      let x_digits = num_digits x in
      if x_digits mod 2 = 0 then split_num x else [ x * 2024 ]
    end
  ;;

  let evolve_all stones = stones |> List.map ~f:evolve |> List.join
end

module M = struct
  (* Type to parse the input into *)
  type t = int list

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs = inputs |> String.strip |> String.split ~on:' ' |> List.map ~f:int_of_string

  (* Run part 1 with parsed inputs *)
  let part1 inputs =
    let final =
      List.range 0 25 |> List.fold ~init:inputs ~f:(fun stones _ -> Stone.evolve_all stones)
    in
    List.length final |> string_of_int |> print_endline
  ;;

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example = "125 17"

(* Expect test for example input *)
let%expect_test "small evolve" =
  let parsed = "0 1 10 99 999" |> M.parse in
  let evolved = Stone.evolve_all parsed in
  List.iter evolved ~f:(fun x -> Printf.printf "%d " x);
  [%expect {| 1 2024 1 0 9 9 2021976 |}]
;;

let%expect_test "part 1" =
  run example ~only_part1:true;
  [%expect {| 55312 |}]
;;

let%expect_test "part 1 small" =
  let parsed = example |> M.parse in

  let final =
    List.range 0 6 |> List.fold ~init:parsed ~f:(fun stones _ -> Stone.evolve_all stones)
  in
  List.length final |> string_of_int |> print_endline;
  [%expect {| 22 |}]
;;

