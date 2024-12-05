open Core

module Xmas = struct
  type pos =
    { x : int
    ; y : int
    }
  [@@deriving show]

  (* list of positions where X (starting a valid xmas) was found *)
  type t = pos list

  let legal_transforms =
    [ (* xmas *)
      [ { x = 0; y = 0 }; { x = 1; y = 0 }; { x = 2; y = 0 }; { x = 3; y = 0 } ]
    ; (* smax *)
      [ { x = 0; y = 0 }; { x = -1; y = 0 }; { x = -2; y = 0 }; { x = -3; y = 0 } ]
    ; (* vertical xmas *)
      [ { x = 0; y = 0 }; { x = 0; y = 1 }; { x = 0; y = 2 }; { x = 0; y = 3 } ]
    ; (* vertical smax *)
      [ { x = 0; y = 0 }; { x = 0; y = -1 }; { x = 0; y = -2 }; { x = 0; y = -3 } ]
    ; (* diagonal xmas top right *)
      [ { x = 0; y = 0 }; { x = 1; y = 1 }; { x = 2; y = 2 }; { x = 3; y = 3 } ]
    ; (* diagonal xmas bottom left *)
      [ { x = 0; y = 0 }; { x = -1; y = -1 }; { x = -2; y = -2 }; { x = -3; y = -3 } ]
    ; (* diagonal xmas bottom right *)
      [ { x = 0; y = 0 }; { x = 1; y = -1 }; { x = 2; y = -2 }; { x = 3; y = -3 } ]
    ; (* diagonal xmas top left *)
      [ { x = 0; y = 0 }; { x = -1; y = 1 }; { x = -2; y = 2 }; { x = -3; y = 3 } ]
    ]
  ;;

  (** Get the word in [tokens] matrix given a list of [transforms], relative to position [x],[y] *)
  let get_word tokens transforms { x; y } =
    let height = Array.length tokens in
    let width = Array.length tokens.(0) in

    List.map transforms ~f:(fun { x = dx; y = dy } ->
      let { x = new_x; y = new_y } = { x = x + dx; y = y + dy } in
      if new_x < 0 || new_x >= width || new_y < 0 || new_y >= height
      then begin
        ' '
      end
      else tokens.(new_y).(new_x))
  ;;

  (* Whether a token list contains a valid XMAS word (or reversed SAMX) *)
  let is_valid = function
    | [ 'X'; 'M'; 'A'; 'S' ] -> true
    | [ 'S'; 'A'; 'M'; 'X' ] -> true
    | _ -> false
  ;;

  (* Check whether relatively to [position], how many valid words in [tokens] when applying any of
     the transforms *)
  let count_words tokens position =
    List.count legal_transforms ~f:(fun tr ->
      (*print_endline "trying transform:";*)
      let word = get_word tokens tr position in
      (*print_endline ("got word:" ^ String.of_char_list word);*)
      let valid = is_valid word in
      (*print_endline ("valid:" ^ string_of_bool valid);*)
      valid)
  ;;
end

module M = struct
  (* Type to parse the input into *)
  type t = char array array [@@deriving show]

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    String.split inputs ~on:'\n'
    |> List.filter ~f:(fun line -> not @@ String.is_empty line)
    |> List.map ~f:String.to_array
    |> List.to_array
  ;;

  (* Run part 1 with parsed inputs *)
  let part1 (tokens : t) =
    let height = Array.length tokens in
    let width = Array.length tokens.(0) in

    let counter = ref 0 in

    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let elem = tokens.(y).(x) in

        (*Printf.printf "access %d %d %c\n" y x elem;*)
        if Char.equal elem 'X'
        then begin
          counter := !counter + Xmas.count_words tokens { x; y }
        end
      done
    done;

    print_endline @@ string_of_int !counter
  ;;

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example =
  {|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX|}
;;

(* Expect test for example input *)
let%expect_test "part 1 example" =
  run example ~only_part1:true;
  [%expect {| 18 |}]
;;
