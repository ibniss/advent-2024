open Core

module Report = struct
  (** Represents a sequence of reactor levels *)
  type t = int list

  (** Direction of change between consecutive levels *)
  type mode =
    | Increasing
    | Decreasing

  (** Parse a raw [line] into a report *)
  let parse_raw line : t =
      line |> String.split ~on:' ' |> List.map ~f:int_of_string

  (** Checks if [report] is safe *)
  let is_safe (report : t) =
    let rec aux mode = function
      | a :: b :: tl ->
        let diff_raw = b - a in
        (* if the number is the same, fail *)
        if diff_raw = 0
        then false
        else begin
          let diff = abs diff_raw in
          let valid_diff = diff >= 1 && diff <= 3 in
          (* if the diff is not valid, fail *)
          if not valid_diff
          then false
          else begin
            match mode with
            | None -> begin
              let new_mode = if b > a then Some Increasing else Some Decreasing in
              aux new_mode (b :: tl)
            end
            | Some Increasing -> b > a && aux mode (b :: tl)
            | Some Decreasing -> b < a && aux mode (b :: tl)
          end
        end
      (* Matches empty list or single element list, both are fine *)
      | _ -> true
    in

    aux None report
  ;;

  (** Create a [report] variant [without] a given level *)
  let create_variant (report : t) ~without : t =
    List.filteri report ~f:(fun idx _ -> idx <> without)
  ;;

  (** Checks if [report] is safe, allowing one bad level *)
  let is_safe_dampened (report : t) =
    if is_safe report
    then true
    else begin
      let max_idx = List.length report in

      (** Check if report is safe while removing the element at [idx] *)
      let rec try_removing_at idx =
        if idx > max_idx
        then false
        else begin
          let variant = create_variant report ~without:idx in
          is_safe variant || try_removing_at (idx + 1)
        end
      in

      try_removing_at 0
    end
  ;;
end

module M = struct
  (* Type to parse the input into *)
  type t = Report.t list

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
      String.split_lines inputs |> List.map ~f:Report.parse_raw

  (* Run part 1 with parsed inputs *)
  let part1 reports =
    List.count reports ~f:Report.is_safe |> string_of_int |> print_endline

  (* Run part 2 with parsed inputs *)
  let part2 reports =
    List.count reports ~f:Report.is_safe_dampened |> string_of_int |> print_endline
end

include M
include Day.Make (M)

(* Example input *)
let example = "\
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9\
"


(* Expect tests for example input *)
let%expect_test "part 1" =
  run example ~only_part1:true;
  [%expect {| 2 |}]
;;

let%expect_test "part 2" =
  run example ~only_part2:true;
  [%expect {| 4 |}]
;;
