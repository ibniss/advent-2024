open Core

module Vec = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving compare, sexp, show]

  let make ~x ~y : t = { x; y }
  let equal { x; y } { x = x'; y = y' } = x = x' && y = y'
  let add { x; y } { x = x'; y = y' } = { x = x + x'; y = y + y' }
  let sub { x; y } { x = x'; y = y' } = { x = x - x'; y = y - y' }
end
