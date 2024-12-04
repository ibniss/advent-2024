open Core

module Lexer = struct
  (** Represents a token *)
  type t =
    | Mul
    | LParen
    | RParen
    | Comma
    | Num of int
    | Invalid
  [@@deriving show]

  let tokenize chars =
    (* alias <= for chars to << so we can use <= for ints *)
    let ( << ) = Char.( <= ) in
    let rec aux acc = function
      | 'm' :: 'u' :: 'l' :: rest -> aux (Mul :: acc) rest
      | '(' :: rest -> aux (LParen :: acc) rest
      | ')' :: rest -> aux (RParen :: acc) rest
      | ',' :: rest -> aux (Comma :: acc) rest
      (* number matched, match rest of the number*)
      | c :: rest when '0' << c && c << '9' -> begin
        let rec match_number num_acc = function
          | d :: rest when '0' << d && d << '9' && List.length num_acc < 3 ->
            match_number (d :: num_acc) rest
          | remaining ->
            let num = num_acc |> List.rev |> String.of_list |> int_of_string in
            Num num :: acc, remaining
        in

        let new_acc, remaining = match_number [ c ] rest in
        aux new_acc remaining
      end
      | _ :: rest -> aux (Invalid :: acc) rest
      | [] -> acc
    in

    aux [] chars |> List.rev
  ;;
end

module Parser = struct
  (** Represents a valid Mul operation *)
  type t = int * int

  (** parse [tokens] into a list of found Mul operations *)
  let parse tokens =
    let open Lexer in
    let rec aux acc = function
      | Mul :: LParen :: Num x :: Comma :: Num y :: RParen :: rest -> aux ((x, y) :: acc) rest
      | _ :: rest -> aux acc rest
      | [] -> acc
    in
    aux [] tokens
  ;;

  (** Evaluate a Mul([x],[y]) *)
  let eval (x, y) = x * y
end

module M = struct
  (* Type to parse the input into *)
  type t = Lexer.t list

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs = Lexer.tokenize (String.to_list inputs)

  (* Run part 1 with parsed inputs *)
  let part1 tokens =
    let sum = Parser.parse tokens |> List.map ~f:Parser.eval |> List.fold ~init:0 ~f:( + ) in
    print_endline @@ string_of_int sum
  ;;

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let example_triple =
  "^+'*>,,why()mul(229,919)&$-#^~mul(187,600)@<select()mul(430,339)<)*/-when()%mul(248,922)~"
;;

let example_invalid = "mul ( 2 , 4 )"

(* Expect test for example input *)
let%expect_test "part 1" =
  run example ~only_part1:true;
  [%expect {| 161 |}]
;;

let%expect_test "example_triple" =
  run example_triple ~only_part1:true;
  [%expect {| 697077 |}]
;;

let%expect_test "example_invalid" =
  run example_invalid ~only_part1:true;
  [%expect {| 0 |}]
;;
