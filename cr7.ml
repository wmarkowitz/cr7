open CS51Utils ;;
module NLS = NativeLazyStreams ;; 

(* See why these might not work! *)

type 'a bad_stream = Cons of 'a * 'a bad_stream ;;

let head (Cons (h, _)) = h ;;

let tail (Cons (_, t)) = t ;;

let rec smap (f: 'a -> 'b) (s: 'a bad_stream) : 'b bad_stream =
  let Cons (h, t) = s in
  Cons (f h, smap f t) ;;

let rec bad_ones = Cons (1, bad_ones) ;;
let rec bad_2_pow init = Cons (init, bad_2_pow (init * 2));;
 
(* THESE DO NOT WORK! *)
let twos = smap ((+) 1) bad_ones ;;
bad_2_pow 3;;
head ( bad_2_pow 3);;


(* A better implementation of streams! *)
type 'a ok_stream_internal = Cons of 'a * 'a ok_stream
  and 'a ok_stream = unit -> 'a ok_stream_internal 

let head (s : 'a ok_stream) : 'a =
  let Cons(h, _t) = s () in
  h ;;

let tail (s : 'a ok_stream) : 'a ok_stream =
  let Cons(_h, t) = s () in
  t ;;

let rec ones : int ok_stream = fun () -> Cons (1, ones) ;;




(* Exercise 1 *)

(* return the first n elements of the stream as a list *)
let first_n (n: int) (str: 'a ok_stream) : 'a list = failwith "not implemented" ;;

(* return a stream where the i'th element of the new stream is f acc a_i 
where a_i is the i'th element of the input stream *)
(* For example: 
  first_n 10 (sfold (+) 0 ones);;
- : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] 
*)
let sfold (f: 'a -> 'b -> 'a) (acc: 'a) (str: 'b ok_stream) : 'a ok_stream = failwith "not implemented" ;;


let rec sfilter (pred : 'a -> bool) (s : 'a ok_stream) : 'a ok_stream =
  fun () ->
    let Cons (h, t) = s () in
    if pred h then Cons (h, sfilter pred t)
    else (sfilter pred t) () ;;

let empty_stream = sfilter (fun x -> x mod 2 == 0) ones ;;

head empty_stream ;;


type 'a thunk_internal = 
  | Unevaluated of (unit -> 'a)
  | Evaluated of 'a ;;

type 'a thunk = 'a thunk_internal ref ;; 

let rec force (t : 'a thunk) : 'a =
  match !t with
  | Evaluated v -> v
  | Unevaluated f -> 
     t := Evaluated (f ());
     force t ;;

let rec tri x = if x <= 1 then 1 else x + tri (x - 1);;


(* Exercise 2 *)

(* Challenge Problem: Implement the functionality of tri above but with a lookup
table that lets us much more quickly find repeated values!!!

This is meant to be difficult!!!

Recommended outline:
  1. Similar to last week, create a function with it's own memory that stores (int * int thunk) values (input, output)
  2. Check to see if our input to tri already has an associated entry in our list above
  3. If it does, then evaluate this found expression
  4. If not, we should create a new entry and add it to the list in memory, then evaluate with our manual expression
*)
let rec tri_with_lookup: int -> int = failwith "Not implemented";; 

(* For testing, try running 

Absbook.call_reporting_time tri_with_lookup 1000;;
Absbook.call_reporting_time tri_with_lookup 1000;;

The second call should have a much faster run time!! *)




(* Exercise 3 *)

let rec ones : int NLS.stream = failwith "not implemented" ;;

let rec nls_first_n (n: int) (str: 'a NLS.stream) : 'a list = failwith "not implemented" ;;

let rec nls_sfold (f: 'a -> 'b -> 'a) (acc: 'a) (str: 'b NLS.stream) : 'a NLS.stream =
  failwith "not implemented" ;;



(* Exercise 4 *)

(* generates a stream such that a_n = f(a_{n-1}) for all values of the stream
  also requires an initial stream value *)
let gen_first_order_recursive (f : 'a -> 'a) (a0: 'a) : 'a NLS.stream = failwith "not implemented" ;;

(* Use this function to reimplement geo from lab this week! *)
let geo (init : float) (mult : float) : float NLS.stream = failwith "not implemented" ;;

(* generates a stream such that a_n = f(a_{n-1}, a_{n-2}) for all values of the stream
  also requires two initial stream values *)
let gen_second_order_recursive (f : 'a -> 'a -> 'a) (a0: 'a) (a1: 'a) : 'a NLS.stream = failwith "not implemented" ;;

(* Use this function to reimplement fibs from lab this week! *)
let fibs : int NLS.stream = failwith "not implemented" ;;
