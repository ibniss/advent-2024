open Core

let findi_last ?(from = None) array ~f =
  let rec loop i =
    if i < 0 then None else if f i array.(i) then Some (i, array.(i)) else loop (i - 1)
  in
  loop @@ Option.value from ~default:(Array.length array - 1)
;;

let findi_from ?(from = None) array ~f =
  let len = Array.length array - 1 in
  let rec loop i =
    if i > len then None else if f i array.(i) then Some (i, array.(i)) else loop (i + 1)
  in
  loop @@ Option.value from ~default:0
;;

module DiskBlock = struct
  type t =
    | FileChunk of
        { id : int
        ; chunk_idx : int
        ; size : int
        }
    | FreeSpace
  [@@deriving compare, sexp, show]

  let file_chunk ~id ~chunk_idx ~size = FileChunk { id; chunk_idx; size }

  let checksum blocks =
    blocks
    |> List.foldi ~init:0 ~f:(fun idx acc b ->
      match b with
      | FreeSpace -> acc
      | FileChunk { id; _ } -> acc + (id * idx))
  ;;
end

module DiskEntry = struct
  type t =
    | File of
        { size : int
        ; id : int
        }
    | Space of { size : int }
  [@@deriving compare, sexp, show]

  let file ~size ~id = File { size; id }
  let space ~size = Space { size }

  let to_blocks = function
    | File { size; id } ->
      List.range 0 size |> List.map ~f:(fun idx -> DiskBlock.file_chunk ~size ~id ~chunk_idx:idx)
    | Space { size } -> List.range 0 size |> List.map ~f:(fun _ -> DiskBlock.FreeSpace)
  ;;
end

module M = struct
  (* Type to parse the input into *)
  type t = DiskEntry.t list [@@deriving compare, sexp, show]

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    inputs
    |> String.strip
    |> String.to_list
    |> List.foldi ~init:[] ~f:(fun idx acc ch ->
      let value = Char.get_digit_exn ch in
      if idx % 2 = 0
      then DiskEntry.file ~size:value ~id:(idx / 2) :: acc
      else DiskEntry.space ~size:value :: acc)
    |> List.rev
    |> List.filter ~f:(fun x ->
      match x with
      | DiskEntry.File { size; _ } -> size > 0
      | DiskEntry.Space { size } -> size > 0)
  ;;

  (* Run part 1 with parsed inputs *)
  let part1 entries =
    let block_arr = entries |> List.map ~f:DiskEntry.to_blocks |> List.join |> List.to_array in

    let rec move file_idx space_idx =
      let open Option.Let_syntax in
      let%bind next_space_idx, next_space =
        findi_from block_arr ~from:(Some space_idx) ~f:(fun _ el ->
          match el with
          | DiskBlock.FreeSpace -> true
          | _ -> false)
      in

      let%bind prev_file_idx, prev_file =
        findi_last block_arr ~from:(Some file_idx) ~f:(fun _ el ->
          match el with
          | DiskBlock.FreeSpace -> false
          | _ -> true)
      in

      (* we crossed each other so stop *)
      if next_space_idx >= prev_file_idx
      then None
      else begin
        (*otherwise, do the reversal *)
        Array.set block_arr next_space_idx prev_file;
        Array.set block_arr prev_file_idx next_space;

        move prev_file_idx next_space_idx
      end
    in

    (* run the moving logic until we can't anymore *)
    ignore @@ move (Array.length block_arr - 1) 0;

    (*Array.iter block_arr ~f:(fun b -> print_endline @@ DiskBlock.show b);*)
    block_arr |> Array.to_list |> DiskBlock.checksum |> string_of_int |> print_endline
  ;;

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example = "2333133121414131402"

(* Expect test for example input *)
let%expect_test "part 1" =
  run example ~only_part1:true;
  [%expect {| 1928 |}]
;;
