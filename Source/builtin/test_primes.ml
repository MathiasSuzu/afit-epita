(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n =
  let rec test t = match modulo n t with
    | 0 -> false
    | j when power t 2 < n ->  test (t+2)
    | _ -> true
  in match n with
     | 0 | 1 -> false
     | 2 | 3 -> true
     | i when modulo n 2 = 0 -> false
     | _ -> test 3;;

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
let is_pseudo_prime p test_seq =
  let rec pseudo_prime_rec p seq =
         match seq with
           [] -> true
         | e::_ when (mod_power e p p) <> (modulo e p) -> false
         | e::l -> pseudo_prime_rec p l
       in if p = 2 || p = 3 then true else pseudo_prime_rec p test_seq;;
