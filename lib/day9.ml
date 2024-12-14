open Core

let findi_last ?(from = None) array ~f =
  let rec loop i =
    if i < 0 then None else if f i array.(i) then Some (i, array.(i)) else loop (i - 1)
  in
  loop @@ Option.value from ~default:(Array.length array - 1)
;;

let findi_last_dyn ?(from = None) (array : 'a Dynarray.t) ~f =
  let rec loop i =
    if i < 0
    then None
    else if f i (Dynarray.get array i)
    then Some (i, Dynarray.get array i)
    else loop (i - 1)
  in
  loop @@ Option.value from ~default:(Dynarray.length array - 1)
;;

let findi_from ?(from = None) array ~f =
  let len = Array.length array - 1 in
  let rec loop i =
    if i > len then None else if f i array.(i) then Some (i, array.(i)) else loop (i + 1)
  in
  loop @@ Option.value from ~default:0
;;

let findi_from_dyn ?(from = None) (array : 'a Dynarray.t) ~f =
  let len = Dynarray.length array - 1 in
  let rec loop i =
    if i > len
    then None
    else if f i (Dynarray.get array i)
    then Some (i, Dynarray.get array i)
    else loop (i + 1)
  in
  loop @@ Option.value from ~default:0
;;

(** Insert a new [value] into [arr] at given [idx]. Shifts elements to the right to accomodate *)
let insert_at (arr : 'a Dynarray.t) (idx : int) (value : 'a) : unit =
  (* Add new element at end first *)
  Dynarray.add_last arr value;
  (* Shift elements right starting from the end *)
  let len = Dynarray.length arr in
  for i = len - 1 downto idx + 1 do
    Dynarray.set arr i (Dynarray.get arr (i - 1))
  done;
  (* Put new value in desired position *)
  Dynarray.set arr idx value
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

  let get_size = function
    | Space { size } -> size
    | File { size; _ } -> size
  ;;

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

    block_arr |> Array.to_list |> DiskBlock.checksum |> string_of_int |> print_endline
  ;;

  (* Run part 2 with parsed inputs *)
  let part2 entries =
    let entry_arr = Dynarray.create () in
    Dynarray.append_list entry_arr entries;

    let rec move file_idx =
      let open Option.Let_syntax in
      (* find next file, from end of arr *)
      let%bind next_file_idx, next_file =
        findi_last_dyn entry_arr ~from:(Some file_idx) ~f:(fun _ el ->
          match el with
          | DiskEntry.Space { size = _ } -> false
          | _ -> true)
      in

      match next_file with
      | DiskEntry.File { size = file_size; _ } -> begin
        (* now consider ALL whitespaces, find first one with matching size *)
        let space_result =
          findi_from_dyn entry_arr ~f:(fun _ el ->
            match el with
            | DiskEntry.Space { size } -> size >= file_size
            | _ -> false)
        in

        (* if we there is no matching space for that file anywhere, keep going *)
        match space_result with
        | None -> move (next_file_idx - 1)
        | Some (next_space_idx, next_space) -> begin
          (* if the matchin space is to the right of the file, keep going *)
          if next_space_idx >= next_file_idx
          then move (next_file_idx - 1)
          else (
            let next_size = DiskEntry.get_size next_space in

            (* if the size was larger than the file *)
            if next_size > file_size
            then (
              (* in place of file, put space of size of file *)
              Dynarray.set entry_arr next_file_idx (DiskEntry.space ~size:file_size);

              (* in place of space, put file + leftover space *)
              Dynarray.set entry_arr next_space_idx next_file;
              (* inserts a new elem *)
              insert_at
                entry_arr
                (next_space_idx + 1)
                (DiskEntry.space ~size:(next_size - file_size)))
            else (
              (* swap *)
              Dynarray.set entry_arr next_space_idx next_file;
              Dynarray.set entry_arr next_file_idx next_space);

            (* recurse *)
            move next_file_idx)
        end
      end
      (* technically we always find a file but type system doesn't know that *)
      | _ -> None
    in

    (* run the moving logic until we can't anymore *)
    ignore @@ move (Dynarray.length entry_arr - 1);

    entry_arr
    |> Dynarray.to_list
    |> List.map ~f:DiskEntry.to_blocks
    |> List.join
    |> DiskBlock.checksum
    |> string_of_int
    |> print_endline
  ;;
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

let%expect_test "part 2" =
  run example ~only_part2:true;
  [%expect {| 2858 |}]
;;
