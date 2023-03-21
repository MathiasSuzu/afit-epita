(** Generating primes *)

open Builtin
open Basic_arithmetics

(** List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.
 *)
let init_eratosthenes n =
  let rec init_rec e = match e with
      e when e = n+1 -> []
    | e when modulo e 2 <> 0 -> e::init_rec (e+1)
    | _ -> init_rec (e+1)
  in 2::init_rec 3 ;;


(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n =
  let l = init_eratosthenes n
  in let rec is_primer n prime = match prime with
         prime when prime*prime > n -> true
       | prime when modulo n prime = 0 -> false
       | _ -> is_primer n (prime+2)
  in let rec test_list list = match list with
        [] -> []
       | e::l when is_primer e 3 = true -> e :: (test_list l)
       | e::l -> test_list l
  in test_list l;;

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let oc = open_out file
    in let rec aux l = match l with
         |e::l -> Printf.fprintf oc "%d\n" e; aux l
         |[] -> close_out oc
  in aux li;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file =
  write_list (eratosthenes n) file;;


(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file =
  create_list (open_in file);;

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  let rec db_rec l = match l with
    |e::l when isprime (e*2+1) && isprime e -> (e,e*2+1)::db_rec l
    |e::l -> db_rec l
    |[] -> []
  in db_rec (eratosthenes limit);;
(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec twin_rec l = match l with
    |e::l when isprime (e+2) && isprime e -> (e,e+2)::twin_rec l
    |e::l -> twin_rec  l
    |[] -> []
  in twin_rec (eratosthenes limit);;
