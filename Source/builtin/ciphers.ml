(** Ciphers
    Builtin integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)
let encrypt_cesar k m b =
  let rec encrypt k m b = match m with
    | e::l -> modulo (e+k) b :: encrypt k l b
    | [] -> []
  in encrypt k m b;;

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)
let decrypt_cesar k m b =
  let rec decrypt k m b = match m with
    | e::l -> modulo (e-k) b :: decrypt k l b
    | [] -> []
  in decrypt k m b;;


(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  let n = p*q
  in let n_phi = (p-1)*(q-1)
  in let rec e_rec e = match e with
    | e when gcd e n_phi = 1 -> e
    | _ -> e_rec (e+1)
  in let e = e_rec (n_phi-1)
  in let (d,_,_) = bezout e n_phi
  in ((n,e),(n,d));;

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) =
  mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) =
  mod_power m d n;;


(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p =
  let rec rec_g g =
    if mod_power g 2 p <> 1 then (g,p)
    else rec_g (g+2)
  in rec_g 2;;


(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
 let a = Random.int p in
  ((mod_power g a p), a);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let a = Random.int p in
  ((mod_power g a p), ((mod_power kA a p) * msg));;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)

let decrypt_g (msgA, msgB) a (g, p) =
  let k = mod_power msgA a p
 in modulo (msgB * mod_power k (p-2) p) p;;
