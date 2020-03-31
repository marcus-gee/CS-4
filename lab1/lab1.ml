(* A.1 *)

    (* 1.  - : int = 10 *)
    (* 2.  - : float = 10. *)
    (* 3.  - : int = 12 *)
    (* 4.  Error: This expression has type float but an expression was expected 
    of type int.
    The error ocurred because for operations involving floats we need to use 
    operator underloading. So it should be 3.2 +. 4.2 *)
    (* 5.  Error: This expression has type int but an expression was expected of
    type float
    The error ocurred because for operations involving ints we need to use the 
    standard +, so it should be 3 + 4 *)
    (* 6.  Error: This expression has type float but an expression was expected 
    of type int 
    The error occurred because we are trying to add a float using the standard 
    addition operator. *)
    (* 7.  Error: This expression has type int but an expression was expected of
    type float 
    The error occurred because we are trying to add an int using the underloaded
     addition operator. *)
    (* 8.  - : float = 7.2*)
    (* 9.  - : int = 5 *)
    (* 10. - : int = 7 *)
    (* 11. val a : int = 3 *)
    (* 12. val b : int = 4 *)
    (* 13. - : bool = false *)
    (* 14. - : bool = true *)
    (* 15. - : bool = false 
    This is different from the previous one. The == checks if values are the 
    same in memory *)
    (* 16. - : (int * int * int) list = [(1, 2, 3)] *)
    (* 17. - : (int * int * int) list = [(1, 2, 3)]
    Because the elements in the list are comma separated, OCaml interprets this 
    as being a list of 1 3-tuple instead of a 3 element list. *)
    (* 18. - : int = 4 *)
    (* 19. Error: Syntax error 
    The AND in OCaml should be typed as &&, not and. *) 
    (* 20. - : int = 6 *)
    (* 21. - : int = 4 *
    This is different from the previous case (A.1.20) because in the above one, 
    the addition of 2 is outside what get returned from the evaluation of the 
    conditional, whereas in this one, the addition of 2 only occurs if the 
    conditional is false. *)
    (* 22. - : int = 6 *)
    (* 23. Error: This expression has type int but an expression was expected of
    type unit because it is in the result of a conditional with no else branch.
    This error occurs because the standard conditional in OCaml is of the from 
    if/then/else, however, here there is no else so the return values of the 
    conditional do not match (int and unit), thus causing an error. *)


(* A.2 *)
let sum_of_squares_of_two_largest x y z = 
    let square n = n * n in 
    let sum_squares a b = square a + square b in 
    
    if x > z && y > z then sum_squares x y 
    else if x > y && z > y then sum_squares x z
    else sum_squares y z
    
  
(* A.3 *)
(* If b is a positive value, then we add it to a, else if b is negative (or 0),
then we subtract the number from a. Thus, the function adds the 
magnitude/absolute value of b to a. *)


(* B.1 *)
(* With an interpreter that uses normal-order evaluation, he will observe an 
output of 0, whereas with an interpreter using applicative-order evaluation, he 
will be stuck in an infinite loop of p () continuously calling itself. *)

 
(* B.2 *) 
(* Assuming the interpreter uses applicative-order evaluation, all of the
arguments of new_if will need to be evaluated. This will continuously and 
recursively call sqrt_iter causing an infinite loop. *)


(* B.3 *)
    (* 1. According to the definitions of recursive and iterative processes, 
     add_a is a recursive process and add_b is an iterative process. *)
    
    (* 2. 
      let rec add_a a b =
      if a = 0
         then b
         else inc (add_a (dec a) b)
         
    Desugar this to:
    let rec add_a =
        fun a b ->
            if a = 0
                then b
                else inc (add_a (dec a) b)

    Bind the name add_a to the value:
    fun a b ->
        if a = 0
            then b
            else inc (add_a (dec a) b)
    
    evaluate 2 -> 2
    evaluate 5 -> 5
    evaluate add_a
        apply (fun a b -> if ...) to 2, 5
        substitute 2 for a, 5 for b in (if ...)
            -> if 2 = 0 then 5 else inc (add_a (dec 2) 5)
            evaluate (if 2 = 0 then 5 else inc (add_a (dec 2) 5))
                if is a special form, so evaluate the first operand:
                    evaluate (2 = 0)
                    evaluate 2 -> 2
                    evaluate 0 -> 0
                    evaluate = -> =
            apply = to 2, 0 -> false
        first argument of if is false, so evaluate the third operand:
        evaluate inc (add_a (dec 2) 5)
            evaluate add_a (dec 2) 5
                evaluate (dec 2)
                evaluate 2 -> 2
                evaluate dec -> (fun a -> ...)
                apply dec to 2
                    substitute 2 for a in the expression a-1 -> 2-1 
                    evaluate 2 - 1 -> 1
                evaluate 5 -> 5
                evaluate add_a -> (fun a b -> ...)
                apply (fun a b -> if ...) to 1, 5
                    substitute 1 for a, 5 for b in (if ...)
                    -> if 1 = 0 then 5 else inc (add_a (dec 1) 5)
                    evaluate (if 1 = 0 then 5 else inc (add_a (dec 1) 5))
                        if is a special form, so evaluate the first operand:
                            evaluate (1 = 0)
                            evaluate 1 -> 1
                            evaluate 0 -> 0
                            evaluate = -> =
                            apply = to 1, 0 -> false
                    first argument of if is false, so evaluate the third operand:
                    evaluate inc (add_a (dec 1) 5)
                        evaluate add_a (dec 1) 5
                        evaluate (dec 1)
                        evaluate 1 -> 1
                        evaluate dec -> (fun a -> ...)
                            apply dec to 1
                            substitute 1 for a in the expression a-1 -> 1-1 
                            evaluate 1 - 1 -> 0
                            evaluate 5 -> 5
                          
                            evaluate add_a -> (fun a b -> ...)    
                            apply (fun a b -> if ...) to 0, 5
                                substitute 0 for a, 5 for b in (if ...)
                                -> if 0 = 0 then 5 else inc (add_a (dec 0) 5)
                                evaluate (if 0 = 0 then 5 else inc (add_a (dec 0) 5))
                                    if is a special form, so evaluate the first operand:
                                        evaluate (0 = 0)
                                        evaluate 1 -> 1
                                        evaluate 0 -> 0
                                        evaluate = -> =
                                        apply = to 0, 0 -> true
                            first argument of if is true, so evaluate the second operand: 5
                              
                     evaluate inc (inc (5))
                     evaluate (inc 5)
                         evaluate 5 -> 5
                         evaluate inc -> (fun a ->...)
                         apply inc to 5 
                            substitute 5 for a in the expression a+1 -> 5+1 
                            evaluate 5 + 1 -> 6 
            evaluate (inc 6)
            evaluate 6 -> 6
            evaluate inc -> (fun a ->...)
            apply inc to 6 
                substitute 6 for a in the expression a+1 -> 6+1 
                evaluate 6 + 1 -> 7
             
            result: 7   
*)
    
    (* 3. 
    let rec add_b a b =
      if a = 0
         then b
         else add_b (dec a) (inc b)

    Desugar this to:
    let rec add_b =
      fun a b ->
        if a = 0
           then b
           else add_b (dec a) (inc b)
    Bind the name add_b to the value:
    fun a b ->
        if a = 0
           then b
           else add_b (dec a) (inc b)

    
    evaluate 2 -> 2
    evaluate 5 -> 5
    evaluate add_b
        apply (fun a b -> if ...) to 2, 5
        substitute 2 for a, 5 for b in (if ...)
        -> if 2 = 0 then 5 else add_b (dec 2) (inc 5)
            evaluate (if 2 = 0 then 5 else add_b (dec 2) (inc 5))
            if is a special form, so evaluate the first operand:
                evaluate (2 = 0)
>>>             evaluate 2 -> 2
>>>             evaluate 0 -> 0
>>>             evaluate = -> =
                apply = to 2, 0 -> false
            first argument of if is false, so evaluate the third operand:
                evaluate (add_b (dec 2) (inc 5))
                evaluate (dec 2)
>>>             evaluate 2 -> 2
>>>             evaluate dec -> (fun a -> ...)
                apply dec to 2
>>>                 substitute 2 for a in the expression a-1 -> 2-1 
>>>                 evaluate 2 - 1 -> 1
                evaluate (inc 5)
>>>             evaluate 5 -> 5
>>>             evaluate inc -> (fun a ->...)
                apply inc to 5 
>>>                 substitute 5 for a in the expression a+1 -> 5+1 
>>>                 evaluate 5 + 1 -> 6   
>>>         evaluate add_b -> (fun a b -> ...)           
                apply (fun a b -> if ...) to 1, 6
                substitute 1 for a, 6 for b in (if ...)
                -> if 1 = 0 then 6 else add_b (dec 1) (inc 6)
                    evaluate (if 1 = 0 then 6 else add_b (dec 1) (inc 6))
                    if is a special form, so evaluate the first operand:
                        evaluate (1 = 0)
>>>                     evaluate 1 -> 1
>>>                     evaluate 0 -> 0  
>>>                     evaluate = -> =              
                        apply = to 1, 0 -> false
              first argument of if is false, so evaluate the third operand:
                  evaluate (add_b (dec 1) (inc 6))
                      evaluate (dec 1)
>>>                   evaluate 1 -> 1
>>>                   evaluate dec -> (fun a -> ...)
                      apply dec to 1
>>>                     substitute 1 for a in the expression 1-1 -> 1-1 
>>>                     evaluate 1 - 1 -> 0                     
                      evaluate (inc 6)
>>>                   evaluate 6 -> 6
>>>                   evaluate inc -> (fun a ->...)
                      apply inc to 6 
>>>                       substitute 6 for a in the expression a+1 -> 6+1 
>>>                       evaluate 6 + 1 -> 7      
>>>                   evaluate add_b -> (fun a b -> ...)                 
                      apply (fun a b -> if ...) to 0, 7
                          substitute 0 for a, 7 for b in (if ...)
                          -> if 0 = 0 then 7 else add_b (dec 0) (inc 7)
                              evaluate (if 0 = 0 then 7 else add_b (dec 0) (inc 7))
                              if is a special form, so evaluate the first operand:
                                  evaluate (0 = 0)
>>>                               evaluate 7 -> 7
>>>                               evaluate 0 -> 0
>>>                               evaluate = -> =
                          apply = to 0, 0 -> true
                          first argument of if is true, so evaluate the second operand:
                      
                      evaluate 7 -> 7
                      result: 7
*)


(* C.1 *) 
(* This function computes the factorial of the input number, which for a number
n is equal to n * (n-1) * ... * 1. *)
let rec factorial n =
  if n = 0 
    then 1 
    else n * factorial (n - 1)
  
(* a *)
(* This function computes the nth term of the infinite series expansion of e.
The nth term takes the form of 1 / (n-1)! *)
let e_term n = 
    1. /. float_of_int (factorial (n - 1))
  
(* b *)
(* This function computes an approximation of e by summing up the first n+1
terms of the infinite series expansion of e.*)
let rec e_approximation n = 
    if n = 0 
        then 0.
        else e_term n +. e_approximation (n - 1)
    
(* c *)
(* # e_approximation 20;;
    - : float = 2.71828182845904553
   # exp 1.0;;
    - : float = 2.71828182845904509 *)
  
(* d *)
(* # e_approximation 100;;
    - : float = infinity
  We get a value of infinity because the factorial values get so large and
  overflow and we get a value of 0, so when we do e_term of the factorial
  value, which is 0, we get infinity. *)


(* C.2 *)
(* The functions is_even/is_odd returns true or false depending on the parity 
of the input n *)
let rec is_even n = 
    if n = 0 
        then true 
        else is_odd (n - 1)
and is_odd n = 
    if n = 0 
        then false 
        else is_even (n - 1) 
  
  
(* C.3 *)
(* This function evaluates the given function f with a recursive process *)
let rec f_rec n = 
    if n < 3 
        then n 
        else f_rec(n - 1) + (2 * f_rec(n - 2)) + (3 * f_rec(n - 3))
 
(* This function evaluates the given function f with an iterative process *)
let f_iter n = 
    let rec iter a b c d = 
        if d = 0 
        then c 
        else iter (a + 2*b + 3*c) a b (d-1) 
    in    
      iter 2 1 0 n
    
        
(* C.4 *)
let rec pascal_coefficient n m = 
    match n, m with
        | _, _ when (n < 1) || (m < 1) || (m > n) -> raise (Failure "invalid arguments")
        | 1, 1 -> 1
        | _, 1 -> 1
        | _, _ when n = m -> 1
        | _, _ -> pascal_coefficient (n-1) (m-1) + pascal_coefficient (n-1) m
  
  
  
  
  
  
  
