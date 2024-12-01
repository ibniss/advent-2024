open Core

(* Read and split into two lists; note: they are reversed *)
let read_lists () =
  let lines = Stdio.In_channel.read_lines "./inputs/day1_prod.txt" in

  List.fold lines ~init:([], []) ~f:(fun (left_nums, right_nums) line ->
    let items =
      String.split line ~on:' ' |> List.filter ~f:(fun item -> String.is_empty item |> not)
    in

    (* match exactly a two element list *)
    match items with
    | [ first_item; second_item ] ->
      int_of_string first_item :: left_nums, int_of_string second_item :: right_nums
    | _ -> failwith ("Could not find two items in line " ^ line))
;;

let _part_1 () =
  (* First split into two lists; note: they are reversed *)
  let first, second = read_lists () in

  let first_sorted = List.sort first ~compare:Int.compare in
  let second_sorted = List.sort second ~compare:Int.compare in

  (* zip them and sum abs distances *)
  let total_distance =
    List.zip_exn first_sorted second_sorted
    |> List.fold ~init:0 ~f:(fun acc (a, b) ->
      let dist = a - b |> abs in
      dist + acc)
  in
  print_endline ("Total distance: " ^ string_of_int total_distance)
;;

(** Build an occurence map of [list] *)
let occurrence_map list =
  let tbl = Hashtbl.create (module Int) in
  List.iter list ~f:(fun elem ->
    Hashtbl.update tbl elem ~f:(function
      | Some prev -> prev + 1
      | None -> 1));
  tbl
;;

(** Compute score of [list] given the occurence [map] *)
let compute_similarity_score list map =
  List.fold list ~init:0 ~f:(fun acc elem ->
    (* how many times element was present in the other list *)
    let occurences = Hashtbl.find map elem |> Option.value ~default:0 in
    (* score is the number multiplied by occurences *)
    let score = elem * occurences in
    acc + score)
;;

let part_2 () =
  let first, second = read_lists () in
  let second_map = occurrence_map second in
  let score = compute_similarity_score first second_map in
  print_endline ("Score: " ^ string_of_int score)
;;

let () = part_2 ()
