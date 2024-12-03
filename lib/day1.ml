open Core

module Location = struct
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
end

module M = struct
  (* Type to parse the input into *)
  type t = int list * int list

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    inputs
    |> String.split_lines
    |> List.fold ~init:([], []) ~f:(fun (left_nums, right_nums) line ->
      let items =
        String.split line ~on:' ' |> List.filter ~f:(fun item -> String.is_empty item |> not)
      in

      (* match exactly a two element list *)
      match items with
      | [ first_item; second_item ] ->
        int_of_string first_item :: left_nums, int_of_string second_item :: right_nums
      | _ -> failwith ("Could not find two items in line " ^ line))
  ;;

  let part1 (left_nums, right_nums) =
    let left_sorted = List.sort left_nums ~compare:Int.compare in
    let right_sorted = List.sort right_nums ~compare:Int.compare in

    (* zip them and sum abs distances *)
    let total_distance =
      List.zip_exn left_sorted right_sorted
      |> List.fold ~init:0 ~f:(fun acc (a, b) ->
        let dist = a - b |> abs in
        dist + acc)
    in
    print_endline @@ string_of_int total_distance
  ;;

  let part2 (left_nums, right_nums) =
    let second_map = Location.occurrence_map right_nums in
    let score = Location.compute_similarity_score left_nums second_map in
    print_endline @@ string_of_int score
  ;;
end

include M
include Day.Make (M)

(* Example input *)
let example = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"

let%expect_test "part 1" =
  run example ~only_part1:true;
  [%expect {| 11 |}]
;;

let%expect_test "part 2" =
  run example ~only_part2:true;
  [%expect {| 31 |}]
;;
