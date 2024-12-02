open Core
open Stdio

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

let () =
  let lines = Stdio.In_channel.read_lines "./inputs/day2_prod.txt" in
  let reports = List.map lines ~f:Report.parse_raw in
  print_endline "Safe reports:";
  List.count reports ~f:Report.is_safe |> string_of_int |> print_endline;
  print_endline "Safe reports with Problem Dampening:";
  List.count reports ~f:Report.is_safe_dampened |> string_of_int |> print_endline
;;
