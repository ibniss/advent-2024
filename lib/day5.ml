open Core
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

let print_list l = print_endline @@ String.concat ~sep:"," (List.map l ~f:string_of_int)

module Order = struct
  (** map of number -> list of numbers which must come after it *)
  type t = int list IntMap.t

  let print map =
    Map.iteri map ~f:(fun ~key ~data ->
      printf "%d -> %s\n" key (String.concat ~sep:", " (List.map ~f:Int.to_string data)))
  ;;

  let all_elements order =
    (Map.data order |> List.join) @ Map.keys order |> IntSet.of_list |> Set.elements
  ;;

  (** Validate a series of [inputs]. We find if there is an input for which we violated the [order],
      i.e. we've already seen a number which can only occure after that input. *)
  let validate inputs order =
    let rec aux acc = function
      | hd :: tl -> begin
        match Map.find order hd with
        (* numbers which MUST come after the [hd] *)
        | Some afters -> begin
          (* use find to find first after which we already encountered *)
          match List.find afters ~f:(fun el -> List.mem acc el ~equal:Int.equal) with
          (* found, rule violated *)
          | Some _ -> false
          (* not found, recurse *)
          | None -> aux (hd :: acc) tl
        end
        | None -> aux (hd :: acc) tl
      end
      | [] -> true
    in

    aux [] inputs
  ;;

  let rec graph_find_aux (order : t) start needle visited =
    (* break if we find a cycle *)
    if List.mem visited start ~equal:Int.equal
    then false
    else (
      let neighbours = Map.find order start |> Option.value ~default:[] in
      List.find neighbours ~f:(fun n ->
        if n = needle then true else graph_find_aux order n needle (n :: visited))
      |> Option.is_some)
  ;;

  (** Find [needle] in [order] graph starting from the [start] *)
  let graph_find order start needle = graph_find_aux order start needle []

  let sort inputs ~order =
    List.sort inputs ~compare:(fun a b ->
      if graph_find order a b then -1 else if graph_find order b a then 1 else 0)
  ;;
end

module Update = struct
  (** page numbers of the update *)
  type t = int list

  (** Get middle item of the [update] *)
  let get_middle (update : t) =
    let len = List.length update in
    let middle_idx = len / 2 in
    List.nth_exn update middle_idx
  ;;

  let score updates = List.fold updates ~init:0 ~f:(fun acc u -> acc + get_middle u)
end

module M = struct
  (* Type to parse the input into *)
  type t =
    { order : Order.t
    ; pages : Update.t list
    }

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    let lines = String.split inputs ~on:'\n' in
    let empty_index, _ = List.findi_exn lines ~f:(fun _ line -> String.equal line "") in
    let order_lines, input_lines = List.split_n lines empty_index in

    let order =
      order_lines
      |> List.fold ~init:IntMap.empty ~f:(fun acc line ->
        match String.split line ~on:'|' with
        | [ before; after ] ->
          let before_int = int_of_string before in
          let after_int = int_of_string after in
          Map.update acc before_int ~f:(fun prev_list ->
            match prev_list with
            | None -> [ after_int ]
            | Some v -> after_int :: v)
        | _ -> failwith @@ "Invalid line: " ^ line)
    in

    (*Order.print order;*)
    let pages =
      input_lines
      |> List.filter ~f:(fun line -> not (String.is_empty line))
      |> List.map ~f:(fun line -> line |> String.split ~on:',' |> List.map ~f:int_of_string)
    in

    { order; pages }
  ;;

  (* Run part 1 with parsed inputs *)
  let part1 { order; pages } =
    let valid_orders = List.filter pages ~f:(fun page -> Order.validate page order) in
    print_endline @@ string_of_int (Update.score valid_orders)
  ;;

  (* Run part 2 with parsed inputs *)
  let part2 { order; pages } =
    let invalid_orders =
      List.filter pages ~f:(fun page -> not @@ Order.validate page order)
      |> List.map ~f:(fun ord -> Order.sort ord ~order)
    in
    print_endline @@ string_of_int (Update.score invalid_orders)
  ;;
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "47|53\n\
   97|13\n\
   97|61\n\
   97|47\n\
   75|29\n\
   61|13\n\
   75|53\n\
   29|13\n\
   97|29\n\
   53|29\n\
   61|53\n\
   97|53\n\
   61|29\n\
   47|13\n\
   75|47\n\
   97|75\n\
   47|61\n\
   75|61\n\
   47|29\n\
   75|13\n\
   53|13\n\n\
   75,47,61,53,29\n\
   97,61,53,29,13\n\
   75,29,13\n\
   75,97,47,61,53\n\
   61,13,29\n\
   97,13,75,29,47\n"
;;

(* Expect test for example input *)
let%expect_test "part 1" =
  run example ~only_part1:true;
  [%expect {| 143 |}]
;;

let%expect_test "part 2" =
  run example ~only_part2:true;
  [%expect {| 123 |}]
;;
