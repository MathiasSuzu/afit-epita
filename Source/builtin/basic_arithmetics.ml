(** Basic arithmetics with builtin integers *)

open Builtin
(**#use "builtin.ml";;**)

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
 *)
let rec gcd a b =
  let rec pgcd a b r = match (b mod r) with
    | 0 -> if sign(r) = -1 then r*(-1) else r
    | _ -> pgcd (b) (r) (b mod r)
  in pgcd a b (a mod b);;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)

let bezout c d =
let rec coefficient_bezout a b =
  if b = 0 then (1, 0)
  else
    let q = a / b in
    let r = a mod b in
    let (x, y) = coefficient_bezout b r in
    (y, x - q * y)
in let (u,v) = coefficient_bezout c d in (u,v, c*u+d*v) ;;
