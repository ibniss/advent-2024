open Core

module Lexer = struct
  (** Represents a token *)
  type t =
    | Mul
    | LParen
    | RParen
    | Comma
    | Do
    | Dont
    | Num of int
    | Invalid
  [@@deriving show]

  (** Given a [first_digit] followed by [rest] chars, match a 1-3 digit number. Returns a tuple of (number, remaining chars) *)
  let match_number first_digit rest =
    let rec aux num_acc = function
      | d :: rest when Char.is_digit d && List.length num_acc < 3 -> aux (d :: num_acc) rest
      | remaining ->
        let num = num_acc |> List.rev |> String.of_list |> int_of_string in
        num, remaining
    in
    aux [ first_digit ] rest
  ;;

  (** Tokenize a list of [chars] into a list of tokens *)
  let tokenize chars =
    let rec aux acc = function
      | 'm' :: 'u' :: 'l' :: rest -> aux (Mul :: acc) rest
      | 'd' :: 'o' :: '(' :: ')' :: rest -> aux (Do :: acc) rest
      | 'd' :: 'o' :: 'n' :: '\'' :: 't' :: '(' :: ')' :: rest -> aux (Dont :: acc) rest
      | '(' :: rest -> aux (LParen :: acc) rest
      | ')' :: rest -> aux (RParen :: acc) rest
      | ',' :: rest -> aux (Comma :: acc) rest
      (* number matched, match rest of the number*)
      | c :: rest when Char.is_digit c -> begin
        let num, remaining = match_number c rest in
        aux (Num num :: acc) remaining
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
  let parse ?(enable_conditionals = false) (tokens : Lexer.t list) =
    let open Lexer in
    let rec aux acc mul_enabled = function
      | Mul :: LParen :: Num x :: Comma :: Num y :: RParen :: rest ->
        if mul_enabled then aux ((x, y) :: acc) mul_enabled rest else aux acc mul_enabled rest
      | Do :: rest when enable_conditionals -> aux acc true rest
      | Dont :: rest when enable_conditionals -> aux acc false rest
      | _ :: rest -> aux acc mul_enabled rest
      | [] -> acc
    in
    aux [] true tokens
  ;;
end

module Evaluator = struct
  let eval (x, y) = x * y

  (** Execute and sum all Mul(x,y) operations.
      Optionally considers conditionals when [enable_conditionals] is true *)
  let execute (operations : Parser.t list) =
    operations |> List.map ~f:eval |> List.fold ~init:0 ~f:( + )
  ;;

  (** Run Evaluator.execute and print the result *)
  let run (operations : Parser.t list) = execute operations |> string_of_int |> print_endline
end

module M = struct
  (* Type to parse the input into *)
  type t = Lexer.t list

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs = Lexer.tokenize @@ String.to_list inputs

  (* Run part 1 with parsed inputs *)
  let part1 tokens = tokens |> Parser.parse |> Evaluator.run

  (* Run part 2 with parsed inputs *)
  let part2 tokens = tokens |> Parser.parse ~enable_conditionals:true |> Evaluator.run
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

let example_two = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let%expect_test "part 2" =
  run example_two ~only_part2:true;
  [%expect {| 48 |}]
;;
