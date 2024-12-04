open Core

(** Signature for a constructed Day module *)
module type S = sig
  val run : ?only_part1:bool -> ?only_part2:bool -> ?enable_time:bool -> string -> unit
end

(** Signature for a Day implementation *)
module type Impl = sig
  type t

  val parse : string -> t
  val part1 : t -> unit
  val part2 : t -> unit
end

(** Run and time function [f], printing time taken labelled by [name] *)
let time ?(enable = true) name f =
  if not enable
  then f ()
  else begin
    let start = Time_ns.now () in
    let result = f () in
    let stop = Time_ns.now () in
    let span = Time_ns.diff stop start |> Time_ns.Span.to_string_hum ~decimals:3 in
    Printf.printf "%s took: %s\n" name span;
    result
  end
;;

(** Functor that takes a Day implementation and outputs a Day module *)
module Make (Impl : Impl) : S = struct
  let run ?(only_part1 = false) ?(only_part2 = false) ?(enable_time = false) inputs =
    let parsed = time "Parsing" ~enable:enable_time (fun () -> Impl.parse inputs) in
    let () =
      if not only_part2 then time "Part1" ~enable:enable_time (fun () -> Impl.part1 parsed)
    in
    let () =
      if not only_part1 then time "Part2" ~enable:enable_time (fun () -> Impl.part2 parsed)
    in
    ()
  ;;
end
