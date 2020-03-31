open Num
let ni = num_of_int

(* A.1 *)
(* Assuming that OCaml is using applicative-order evaluation, then the space
complexity of the function is O(n) becuase the maximum depth of the recursion 
tree is n. Once the tree gets to a depth of n, it starts returning values up the
tree, which makes it so there are less function calls needing to be evaluated. 
The space complexiity is different from the time comlexity because the time 
complexity looks at how many function calls are being made and how long those 
function calls take, whereas the space complexity looks at the depth of the 
recursion tree and the amount of space needed to store any data from the
function calls, which in this worst case is n.*)

(* A.2 *)
(* 1. THe function p is applied 5 times when sine 12.15 is evaluated *)
(* 2. The growth of in space of the sine a function is the ceiling of 
log3(10 * a), so O(log a). We get this because the log base 3 is the number of 
times a mustbe divide so that the angle to 1, then because the function runs 
until the angle is < 0.1, we multiply by 10. Then becuase we cannot divide by a 
fraction of 3, we take the ceiling. The time complexity is O(log a), because a 
is divided by a constant number in every call to sine. *)
 
(* A.3 *)
(* 1 *)
let rec fast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
    match n  with 
      | 0 -> 1
      | x when (is_even x) -> square (fast_expt b (x / 2))
      | _ -> b * fast_expt b (n - 1)

(* 2 *)
let ifast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
  let rec iter b n a = 
    match n  with 
      | 0 -> a
      | x when (is_even x) -> (iter (square b) (n / 2) a)
      | _ -> iter b (n - 1) (b * a) 
    in 
      iter b n 1
    
(* A.4 *)
let rec fast_mult b n =
  let is_even m = m mod 2 = 0 in
  let double m = m * 2 in
  let halve m = m / 2 in
    match n with 
      | 0 -> 0
      | x when (is_even x) -> double (fast_mult b (halve n))
      | _ -> b + fast_mult b (n - 1)


(* A.5 *)
let ifast_mult a b =
  let is_even m = m mod 2 = 0 in
  let double m = m * 2 in
  let halve m = m / 2 in
  let rec iter a b sum = 
    match b with 
      | 0 -> sum
      | x when (is_even x) -> iter (double a) (halve b) sum
      | _ -> iter a (b - 1) (a + sum)
    in 
      iter a b 0
    
(* A.6 *)
(* The worst-case space complexity is O(log n) since every function call n is 
divided by 2, the the maximum depth of the stack is log n. The worst case time
complexity is O(2^(log n)), which is simplified to O(n^log2), because there are 
two recursive calls being called on half of n. *)

(* A.7 *)
(* 1. *)
(* This function represents a linear recursive process because it makes a 
 recursive call that decrements the input by 1 each call. The function is
linear in the size of the input n and makes higher level calls that wait for
the lower level ones to finish. *)

(* 2. *)
(* The space complexity is O(n), where n is the size of the input, becuase
every recursive call waits for the lower one to finish and the maximum 
recursion depth is n. The time complexity is also O(n) since the number of 
calls also depends only on n. *)

(* B.1 *)
(* a *)
  (* 
   * (fun x y -> (x * (2 + y))) 20 (2 * 4) 
   *)

(* b *)
  (* 
   * (fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0
   *)
  

(* c *)
  (*
   * (fun x -> let y = 2 in let z = 3 in x * y * z) 1
   * (fun x -> (fun y -> let z = 3 in x * y * z) 2) 1
   * (fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1
   *) 

(* d *)
  (*
   * (fun x -> let x = 2 in let x = 3 in x * x * x) 1
   * (fun x -> (fun x -> let x = 3 in x * x * x) 2) 1
   * (fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1
   *)

(* B.2 *)
  (*      
  (fun x y -> let y = 14 in let z = 22 in x * y * z) (2 * 10) (3 + 4)
                  
  (fun x y -> (fun y -> let z = 22 in x * y * z) 14) (2 * 10) (3 + 4) 
    
  (fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)
   
  Evaluate 2 * 10 = 20
  Evaluate 3 + 4 = 7
   
  (fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) 20 7  
  
  (fun z -> 20 * 7 * z) 22) 14) 
  Evaluate 20 * 7 = 140
  
  (fun y -> (fun z -> 140 * z) 22) 14
  
  (fun z -> 140 * z) 22
  Evaluate 140 * 22 = 3080      
  *)
 
(* B.3 *)
  (* 
  (fun x y z -> x + y + z) 10 (x * 2) (y + 3)
  Both x and y are unbounded to a value so they cannot be used as arguments to 
  the function yet. This is because of the applicative-order evaluation process
  that OCaml uses, which evaluates the arguments first, but x and y do not have 
  values. 
  To resolve the issue, we could use "let ... in".
  
  let x = 10 in 
  and y = x * 2 in 
  and z = y + 3 
  in x + y + z
  *)
  
(* C.1 *)
let isum term a next b =
  let rec iter a result =
    if a >/ b
       then result 
       else iter (next a) (result +/ term a)
  in
    iter a (ni(0))

(* C.2 *)
(* 1 *)
let rec product_rec term a next b =
  if a >/ b
     then (ni 1)
     else term a */ (product_rec term (next a) next b)

let factorial_rec n = 
  product_rec (fun x -> x) (ni 1) (fun n -> n +/ (ni 1)) n

let pi_product n = 
  (ni 4) */ product_rec (fun x -> 
                                  if (int_of_num x) mod 2 = 0
                                  then (x +/ (ni 2)) //  (x +/ (ni 1))
                                  else (x +/ (ni 1)) //  (x +/ (ni 2))) (ni 1)
                                  (fun x -> x +/ (ni 1)) n 

(* 2 *)  
let product_iter term a next b =
  let rec iter a result =
    if a >/ b
       then result 
    else iter (next a) (result */ term a)
  in
    iter a (ni(1))
    
let factorial_iter n = 
  product_iter (fun x -> x) (ni 1) (fun n -> n +/ (ni 1)) n
  
let pi_approx = 
  float_of_num (pi_product (ni 1000))
  
(* C.3 *)
(* 1 *)
let rec accumulate_rec combiner null_value term a next b =
  if a >/ b
     then null_value
  else combiner (term a) (accumulate_rec combiner null_value term (next a) next b)

(* 2 *)
let accumulate_iter combiner null_value term a next b =
  let rec iter combiner a result =
    if a >/ b
       then result 
    else iter combiner (next a) (combiner result (term a))
  in
    iter combiner a null_value
 
 
let rec sum term a next b =
  if a >/ b
     then (ni 0)
  else term a +/ (sum term (next a) next b)

let rec product term a next b =
  if a >/ b
     then (ni 1)
  else term a */ (product term (next a) next b)

 
 
    
(* C.4 *)
let compose f g = 
  fun x -> f (g x)

(* C.5 *)
let rec repeated f n = 
  if n = 0 
    then fun x -> x
  else compose f (repeated f (n - 1))

(* C.6 *)
let smooth dx f = 
  fun x -> (f(x -. dx) +. f(x) +. f(x +. dx)) /. 3.0

let nsmoothed dx f n= 
  let dx_smooth f = smooth dx f in 
  (repeated dx_smooth n) f 
  
(* D.1 *)
  
let is_prime p = 
  let max = int_of_float(sqrt(float_of_int p)) in 
  let rec iter curr =
    match curr with 
      | x  when x > max -> true
      | x when (p mod x = 0) -> false
      | _ -> iter (curr + 1)
    in if p < 2 
      then false 
    else iter 2

(* D.2 *)
let smallest_prime_factor n = 
  let rec iter curr = 
    if ((is_prime curr = true) && (n mod curr = 0)) 
      then curr
    else iter (curr + 1) in 
      if is_prime n = true || n < 2
        then invalid_arg "number is prime"
      else iter 2
