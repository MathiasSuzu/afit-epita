(** Tweaking OCaml builtin euclidean division

The OCaml built-in euclidian divisions operations do not follow the
standard mathematical conventions. We adapt OCaml available
primitives to suit maths conventions.

 **)

(** Sign function
    @param x integer
*)
let sign x = if x >= 0 then 1 else -1;;

(** Quotient of an integer by a natural number.  This is the quotient
   in euclidiant division sense.  @param a dividend @param b natural
   number you divide by.  *)
let quot_ a b = a/b;;



(** Quotient of two integers. Fully Recursive.
    General case ; explicit by-hand computations. Not efficient enough as
    it is not a low level implementation.
 *)
let quot a b =
  let rec quot_rec_pos a b c =
    if a >= b then
      quot_rec_pos (a-b) b (c+1)
    else
      c
  and quot_rec_neg a b1 c =
    if a < b1 then
      quot_rec_neg (a) (b1-b) (c-1)
    else
      c
  in match (sign(a), b) with
     | (_, 0) -> a
     | ((-1), _) -> quot_rec_neg (a) (0) 0
     |   _  -> quot_rec_pos a b 0;;

(** Modulo of two integers.
    Following euclidean division NOT OCAML DEFAULT. Positive integer
    between 0 (included) and modulo (excluded) resulting from euclidian
    division of entry by modulo.

    OCAML DEFAULT : For negative numbers eucldean result - modulo base.

    @param a input integer
    @param b moduli integer.
 *)
let modulo a b = let result = a mod b in
                 match sign(result) with
                   (-1) -> b+result
                 | _ -> result;;

(** Division of an integer by a natural number. NOT OCAML DEFAULT.
    Division of an integer by a non-zero integer b is the unique couple
    of integers (q, r) such that a = b*q + r and r is in [0, abs b[.
    @param a dividend
    @param b integer you divide by.
*)
let div a b = (quot a b, modulo a b)
