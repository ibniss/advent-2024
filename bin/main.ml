open Core
open Aoc
open Command.Let_syntax

(** Load inputs for a given [day] *)
let load_input day =
  let input_file = Printf.sprintf "inputs/%s.txt" day in
  match Sys_unix.file_exists input_file with
  | `Yes -> Ok (In_channel.read_all input_file)
  | `No -> Error (Printf.sprintf "Input file %s not found" input_file)
  | `Unknown -> Error (Printf.sprintf "Cannot determine if %s exists" input_file)
;;

let days =
  String.Map.of_alist_exn
    [ "1", (module Day1 : Day.S)
    ; "2", (module Day2)
    ; "3", (module Day3)
    ; "4", (module Day4)
    ; "5", (module Day5)
    ; "6", (module Day6)
    ]
;;

let get_day_module day =
  match Map.find days day with
  | Some m -> Ok m
  | None -> Error (Printf.sprintf "Day %s not implemented" day)
;;

let command =
  Command.basic
    ~summary:"Run Advent of Code solutions"
    begin
      let%map_open day = anon ("day" %: string)
      and only_part1 = flag "-1" no_arg ~doc:"Run only part 1"
      and only_part2 = flag "-2" no_arg ~doc:"Run only part 2" in

      fun () ->
        let inputs =
          match load_input day with
          | Ok contents -> contents
          | Error msg -> failwith msg
        in

        let (module Day : Day.S) =
          match get_day_module day with
          | Ok d -> d
          | Error msg -> failwith msg
        in

        Day.run inputs ~only_part1 ~only_part2
    end
;;

let () = Command_unix.run command

(* TODO: timing parts?*)
