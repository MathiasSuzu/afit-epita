(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n =
  let rec power x n =
  if n = 0 then
    1
  else
    x * power x (n - 1)
  in power x n;;

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
  let rec fast_power x n =
    match n with
    | 0 -> 1
    | n when (modulo n 2) = 0 -> let y = fast_power x (n / 2) in y * y
    | _ -> x * fast_power x (n - 1)
  in fast_power x n;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
  let rec pow_rec x n =
    match n with
    | 0 -> 1
    | n when (modulo n 2) = 0 -> let y = pow_rec x (n / 2) in modulo (y * y) m
    | _ -> modulo (x * pow_rec x (n-1)) m
  in pow_rec x n;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  let rec pow_rec x n  =
    match n with
    | 0 -> 1
    | n when n = p-1 && modulo x p <> 0 -> 1
    | n when (modulo n 2) = 0 -> let y = pow_rec x (n / 2) in modulo (y * y) p
    | _ -> modulo (x * pow_rec x (n-1)) p
  in pow_rec x n ;;
