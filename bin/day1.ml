open Core

let () =
  let lines = Stdio.In_channel.read_lines "./inputs/day1_prod.txt" in

  (* First split into two lists; note: they are reversed *)
  let first, second =
    List.fold lines ~init:([], []) ~f:(fun (fst, snd) line ->
      let items =
        String.split line ~on:' ' |> List.filter ~f:(fun item -> String.is_empty item |> not)
      in

      (* match exactly a two element list *)
      match items with
      | [ first_item; second_item ] ->
        int_of_string first_item :: fst, int_of_string second_item :: snd
      | _ -> failwith ("Could not find two items in line " ^ line))
  in

  let first_sorted = List.sort first ~compare:( - ) in
  let second_sorted = List.sort second ~compare:( - ) in

  (* zip them and sum abs distances *)
  let total_distance =
    List.zip_exn first_sorted second_sorted
    |> List.fold ~init:0 ~f:(fun acc (a, b) ->
      let dist = a - b |> abs in
      dist + acc)
  in
  print_endline ("Total distance: " ^ string_of_int total_distance)
;;
