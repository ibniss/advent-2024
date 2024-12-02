open Core

module Report = struct
  (*type t = level list*)
  (*and level = int*)

  type mode = Increasing | Decreasing

  type pairwise_accum = { pairs: (int * int) list; last_num: int option }

  let pairwise list =
    let result = List.fold list ~init:{ pairs = []; last_num = None} ~f:(fun ({ pairs; last_num } as acc) elem ->
      match last_num with
      | None -> { acc with last_num = Some elem }
      | Some prev -> { pairs = (prev, elem) :: pairs; last_num = Some elem }
    ) in
    List.rev result.pairs

  let parse_raw line =
    line |> String.split ~on:' ' |> List.map ~f:int_of_string

  let is_safe report =
    (* keep track of current mode *)
    let current_mode : (mode option) ref = ref None in
    let pairs = pairwise report in
    List.for_all pairs ~f:(fun (fst, snd) ->
      let diff_raw = snd - fst in

      (* if the number is the same we can just short-circuit, it's invalid *)
      if diff_raw = 0 then
        false
      else begin
        let diff = abs diff_raw in
        let valid_diff = diff >= 1 && diff <= 3 in
        let valid_mode = match !current_mode with
          (* first time, initialize mode *)
          | None -> begin
              current_mode := if diff_raw > 0 then Some Increasing else Some Decreasing;
              true
          end
          (* if we're increasing every diff needs to be positive *)
          | Some Increasing -> diff_raw > 0
          (* if we're decreasing every diff needs to be negative *)
          | Some Decreasing -> diff_raw < 0
        in
        valid_diff && valid_mode
      end
    )
end

let () =
  let lines = Stdio.In_channel.read_lines "./inputs/day2_prod.txt" in
  let reports = List.map lines ~f:Report.parse_raw in
  List.count reports ~f:Report.is_safe |> string_of_int |> print_endline;
  (*List.iter reports ~f:(fun report ->*)
  (*    print_endline "Report:";*)
  (*    Report.pairwise report |> List.iter ~f:(fun (fst, snd) ->*)
  (*      Stdlib.print_char '(';*)
  (*      Stdlib.print_int fst;*)
  (*      Stdlib.print_char ',';*)
  (*      Stdlib.print_int snd;*)
  (*      Stdlib.print_char ')';*)
  (*    );*)
  (*    print_endline ("Is valid: " ^ string_of_bool (Report.is_safe report));*)
  (*    print_endline "";*)
  (*);*)

