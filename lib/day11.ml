open Core
module IntTable = Hashtbl.Make (Int)
module IntMap = Map.Make (Int)

(* list-based solution *)
module Stone = struct
  (* memo for single evolutions *)
  let memo_table = IntTable.create ()

  let num_digits n =
    if n = 0 then 1 else Float.log10 (Float.of_int (abs n)) |> Float.to_int |> ( + ) 1
  ;;

  let do_evolve x =
    if x = 0
    then [ 1 ]
    else begin
      let x_digits = num_digits x in
      if x_digits mod 2 = 0
      then (
        let half_digit = x_digits / 2 in
        let divisor = Int.pow 10 half_digit in
        let first_part = x / divisor in
        let second_part = x mod divisor in
        [ first_part; second_part ])
      else [ x * 2024 ]
    end
  ;;

  let evolve (x : int) : int list =
    Hashtbl.find_or_add memo_table x ~default:(fun () -> do_evolve x)
  ;;
end

module StoneMap = struct
  (** map of stone -> number of occurences of that stone *)
  type t = int IntMap.t

  let of_list stones : t =
    stones
    |> List.group ~break:( <> )
    |> List.fold ~init:IntMap.empty ~f:(fun acc group ->
      let key = List.hd_exn group in
      let data = List.length group in
      Map.set acc ~key ~data)
  ;;

  let evolve (stones : t) : t =
    (* fold over existing map and create a new one *)
    stones
    |> Map.fold ~init:IntMap.empty ~f:(fun ~key ~data acc ->
      key
      |> Stone.evolve
      |> List.fold ~init:acc ~f:(fun acc new_stone ->
        let result =
          Map.update acc new_stone ~f:(function
            | Some v -> v + data
            | None -> data)
        in
        result))
  ;;

  let evolve_times (stones : t) ~times =
    List.range 0 times |> List.fold ~init:stones ~f:(fun step _ -> evolve step)
  ;;

  let stone_count (stones : t) = Map.data stones |> List.fold ~init:0 ~f:( + )
end

module M = struct
  (* Type to parse the input into *)
  (** map of stone -> number of occurences of that stone *)
  type t = StoneMap.t

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    inputs |> String.strip |> String.split ~on:' ' |> List.map ~f:int_of_string |> StoneMap.of_list
  ;;

  (* Run part 1 with parsed inputs *)
  let part1 inputs =
    let final = StoneMap.evolve_times inputs ~times:25 in
    StoneMap.stone_count final |> string_of_int |> print_endline
  ;;

  (* Run part 2 with parsed inputs *)
  let part2 inputs =
    let final = StoneMap.evolve_times inputs ~times:75 in
    StoneMap.stone_count final |> string_of_int |> print_endline
  ;;
end

include M
include Day.Make (M)

(* Example input *)
let example = "125 17"

let%expect_test "part 1" =
  run example ~only_part1:true;
  [%expect {| 55312 |}]
;;

let%expect_test "part 1 small" =
  let parsed = example |> M.parse in

  let final = StoneMap.evolve_times parsed ~times:6 in
  StoneMap.stone_count final |> string_of_int |> print_endline;
  [%expect {| 22 |}]
;;
