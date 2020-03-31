(* A. 1 *)
(* a record type called point, containing two (floating-point) numbers (with
 *  field names x and y *)
type point = { x_val : float;  y_val: float } 
let make_point x y = { x_val = x; y_val = y }
let get_coords { x_val = x; y_val = y } = (x, y)
let get_x { x_val = x } = x
let get_y { y_val = y } = y

(* a record type called segment containing two points: a starting point (a
 * field called startp) and an ending point (a field called endp) *)
type segment = { startp: point; endp: point }
let make_segment a b = { startp = a; endp = b }
let get_points { startp = a; endp = b } = (a, b)
let get_startp { startp = a } = a 
let get_endp { endp = b } = b

(* this function will take a single segment as its only argument and return a
 * point which is the midpoint of the line segment *)
let midpoint_segment s = 
  make_point
     ((get_x (get_startp s) +. get_x (get_endp s) ) /. 2.0)
     ((get_y (get_startp s) +. get_y (get_endp s) ) /. 2.0)

(* this function will take a single segment as its only argument and return a
 * float which is the length of the line segment *)
let segment_length s = 
    let square x = x *. x in 
    let x1 = get_x (get_startp s) in 
    let x2 = get_x (get_endp s) in 
    let y1 = get_y (get_startp s) in 
    let y2 = get_y (get_endp s) in 
    sqrt (square (x2 -. x1) +. square (y2 -. y1))
  
(* this function will take a single point as its only argument and print its
 * representation to the terminal *)
let print_point p = 
    Printf.printf "(%g, %g)\n" (get_x p) (get_y p)
  
   
(* A.2 *)
(* first rectangle representation *)
type rectangle = { lower_left: point; upper_right: point }
let make_rectangle ll ur = { lower_left = ll; upper_right = ur }
let rectangle_lower_segment { lower_left = ll; upper_right = ur } = 
                             make_segment ll (make_point (get_x ur) (get_y ll))
                              
let rectangle_upper_segment { lower_left = ll; upper_right = ur } = 
                             make_segment ur (make_point (get_x ll) (get_y ur))
                              
let rectangle_left_segment { lower_left = ll; upper_right = ur } = 
                             make_segment ll (make_point (get_x ll) (get_y ur))
                             
let rectangle_right_segment { lower_left = ll; upper_right = ur } = 
                             make_segment ur (make_point (get_x ur) (get_y ll))

let rectangle_perimeter rect = 
    segment_length (rectangle_upper_segment rect) +.
    segment_length (rectangle_lower_segment rect) +.
    segment_length (rectangle_left_segment rect) +.
    segment_length (rectangle_right_segment rect)
                             
let rectangle_area rect =
    segment_length (rectangle_lower_segment rect) *.
    segment_length (rectangle_right_segment rect)

(* second rectangle representation *)
type rectangle2 = { low_x: float; low_y: float; up_x: float; up_y: float }
let make_rectangle2 lx ly ux uy = 
                    { low_x = lx; low_y = ly; up_x = ux; up_y = uy }
let rectangle_lower_segment2 { low_x = lx; low_y = ly; up_x = ux; up_y = uy } = 
                             make_segment (make_point lx ly) (make_point ux ly)
                              
let rectangle_upper_segment2 { low_x = lx; low_y = ly; up_x = ux; up_y = uy } = 
                             make_segment (make_point lx uy) (make_point ux uy)
                              
let rectangle_left_segment2 { low_x = lx; low_y = ly; up_x = ux; up_y = uy } = 
                             make_segment (make_point lx ly) (make_point lx uy)
                             
let rectangle_right_segment2 { low_x = lx; low_y = ly; up_x = ux; up_y = uy } = 
                             make_segment (make_point ux ly) (make_point ux uy)
                            
let rectangle_perimeter2 rect = 
    segment_length (rectangle_upper_segment2 rect) +.
    segment_length (rectangle_lower_segment2 rect) +.
    segment_length (rectangle_left_segment2 rect) +.
    segment_length (rectangle_right_segment2 rect)
                             
let rectangle_area2 rect =
    segment_length (rectangle_lower_segment2 rect) *.
    segment_length (rectangle_right_segment2 rect)
                           

(* A.3 *) 
let make_pair x y = fun m -> m x y
let first z = z (fun x y -> x) 
let second z = z (fun x y -> y)  

(* verify that first (make_pair x y) yields x *)
(* 
 * first (make_pair a b)
 *      evaluate make_pair x y 
 *      first (fun m -> m x y)
 *      evaluate first z to fun z -> (fun x y -> x)
 *      fun (fun x y -> x) -> (fun x y -> x) x y -> x
 *       
 * second (make_pair 1 2)      
 *      evaluate make_pair 1 2
 *          evaluate 1 -> 1
 *          evaluate 2 -> 2
 *          evaluate make_pair to fun x y -> (fun m -> m x y)
 *          apply (fun x y -> ... ) to 1, 2
 *          fun m -> m 1 2
 *      evaluate second z to fun z -> (fun x y -> y)
 *      apply (fun z -> ...) to fun m -> m 1 2
 *          (fun m -> m 1 2) -> (fun x y -> y)
 *          apply fun m -> m 1 2 to fun x y -> y
 *              (fun x y -> y) 1 2
 *              evaluate 1 -> 1
 *              evaluate 2 -> 2
 *              apply (fun x y -> ... ) to 1, 2
 *                  return 2
 *)
 

(* A.4 *)
let pow a b = 
    let rec iter a b value = 
        match b with 
        | 0 -> value
        | _ -> iter a (b - 1) (a * value) 
    in
        iter a b 1
        
let int_log a c =  
    let rec iter a c value = 
        match c with 
        | 1 -> value 
        | x when x mod a = 0 -> iter a (c / a) (value + 1)
        | _ -> value 
    in 
        iter a c 0
        
let make_pairi a b = 
    (pow 2 a ) * (pow 3 b)
    
let firsti n = 
    (int_log 2 n)
    
let secondi n = 
    (int_log 3 n)
    
        
(* A.5 *) 
(* first representation *)
let zero = []

let is_zero = function
  | [] -> true
  | () :: _ -> false

let succ u = () :: u

(* This function takes one argument (a unary representation of an integer) and
 * returns a unary integer one less than argument the *)
let prev u = 
    match u with 
    | [] -> failwith "prev of empty list does not exist"
    | h :: t -> t

(* This function takes one argument (an OCaml int) and returns a unary number
 * representation of the same integer *)
let integer_to_unary n = 
    let rec iter n lst =
        if n = 0 
            then lst 
        else 
            iter (n - 1) (succ lst)
     in
        iter n zero
        
(* This function takes one argument (a unary representation of an integer) and
 * returns the corresponding OCaml int *)
let unary_to_integer u = 
    let rec iter u count = 
        if is_zero u
            then count
        else 
            iter (prev u) (count + 1)
    in 
        iter u 0        

(* is function takes two arguments, both of which are unary representations of
 * integers. It returns the result of adding the two integers together. The
 * return value is a unary representation of an integer, not an OCaml int *)
let rec unary_add u1 u2 = 
    if is_zero u2
        then u1
    else unary_add (succ u1) (prev u2)  

(* second representation *)
type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = function
  | Zero -> true
  | Succ _ -> false

let succ' u = Succ u

(* no we dont really have to make changes*)
(* This function takes one argument (a unary representation of an integer) and
 * returns a unary integer one less than argument the *)
let prev' u = 
    match u with 
    | Zero -> failwith "prev of empty list does not exist"
    | Succ x -> x

let rec integer_to_unary' = function 
    | 0 -> zero' 
    | n -> succ' (integer_to_unary' (n - 1))
        
        
(* This function takes one argument (a unary representation of an integer) and
 * returns the corresponding OCaml int *)
let rec unary_to_integer' = function
    | Zero -> 0
    | Succ x -> 1 + unary_to_integer' x
    
(* is function takes two arguments, both of which are unary representations of
 * integers. It returns the result of adding the two integers together. The
 * return value is a unary representation of an integer, not an OCaml int *)
let rec unary_add' u1 u2 = 
    match u1 with 
    | Zero -> u2
    | Succ x -> unary_add' x (succ' u2)
    
                                 
(* A.6 *)
(* zerof = "functional zero"; we call it this so as not to be confused with
   zero or zero' previously defined. *)
let zerof s z = z 

let add1 n s z = s (n s z)
            
let one s z = s z
let two s z = s (s z)
let three s z = s (s (s z))
let four s z = s (s (s (s z)))
let five s z = s (s (s (s (s z))))
let six s z = s (s (s (s (s (s z)))))
let seven s z = s (s (s (s (s (s (s z))))))
let eight s z = s (s (s (s (s (s (s (s z))))))) 
let nine s z = s (s (s (s (s (s (s (s (s z))))))))
let ten s z = s (s (s (s (s (s (s (s (s (s z)))))))))

let add m n s z = m s (n s z)

let church_to_integer c = c (fun x -> x + 1) 0
        

(* A.7 *)
(* For the zerof case, 'a has to be of the form int->int since it takes as 
 * input the successor function, which returns the successor of an int, so 
 * 'b must be an int and so will the return value
 * 
 * For the one case, 'a->'b is int->int, thus both 'a and 'b have to be ints 
 * Even though in church_to_integer, 'a is returned, this ensures that these
 * cases will return an int. *) 


(* B.1 *)
(* 1 *)
type mobile = Mobile of branch * branch  (* left and right branches *)
and branch =
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)
            
let make_mobile l r = Mobile (l, r)
let left_branch (Mobile (l, r)) = l
let right_branch (Mobile (l, r)) = r

let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m) 

let branch_length b = 
    match b with 
    | Weight (l, w) -> l
    | Structure (l, m) -> l
    
let branch_structure b = 
    match b with 
    | Weight (l, w) -> `Weight w
    | Structure (l, m) -> `Structure m
  
(* 2 *)  
let rec branch_weight1 b = 
    match b with 
    | Weight (l, w) -> w
    | Structure (l, m) -> total_weight1 m
and total_weight1 (Mobile (l, r)) = (branch_weight1 l) + (branch_weight1 r)

let rec branch_weight2 b = 
    match branch_structure b with 
    | `Weight w -> w
    | `Structure m -> total_weight2 m
and total_weight2 m = 
    branch_weight2 (left_branch m) + branch_weight2 (right_branch m)
            
(* 3 *)
let rec is_balanced m =
    let rec iter b = 
        match branch_structure b with 
        | `Weight w -> true
        | `Structure m -> is_balanced m
    in 
        if (iter (left_branch m)) && (iter (right_branch m)) && 
           (branch_length (left_branch m)) * (branch_weight1 (left_branch m)) = 
           (branch_length (right_branch m)) * (branch_weight1 (right_branch m))
        then true
        else false

(* 4 *)
type mobile'  = { left: branch'; right: branch' }
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

    

let make_mobile' l r = { left = l; right = r }
let make_weight' l w = Branch' (l, Weight' w)
let make_structure' l m = Branch' (l, Structure' m)
let left_branch' { left : branch'; right : branch' } = left
let right_branch' { left : branch'; right : branch' } = right 
let branch_length' (Branch' (l, m)) = l  
let branch_structure' (Branch' (l, n)) = 
    match n with 
    | Weight' w -> `Weight w
    | Structure' m -> `Structure m
    
let rec branch_weight' b = 
    match branch_structure' b with 
    | `Weight w -> w
    | `Structure m -> total_weight' m
and total_weight' m = 
    branch_weight' (left_branch' m) + branch_weight' (right_branch' m)

let rec is_balanced' m = 
    let rec iter b = 
        match branch_structure' b with 
        | `Weight w -> true
        | `Structure m -> is_balanced' m
    in 
    if (iter (left_branch' m)) && (iter (right_branch' m)) && 
       (branch_length' (left_branch' m)) * (branch_weight' (left_branch' m)) = 
       (branch_length' (right_branch' m)) * (branch_weight' (right_branch' m))
    then true
    else false
        

(* B.2 *)
type tree = Tree of elem list
and elem =
  | Num of int
  | Sub of tree
  
let rec square_tree (Tree tr) =
    let rec iter tree_lst =
        match tree_lst with 
        | [] -> []
        | Num n :: t -> Num (n * n) :: (iter t)
        | Sub st :: t -> Sub (square_tree st) :: (iter t)
    in
        Tree (iter tr) 

let rec square_tree' (Tree tr) = 
    let square_elements e = 
        match e with
        | Num n -> Num (n * n)
        | Sub st -> Sub (square_tree' st)
    in 
        Tree (List.map square_elements tr)
    

(* B.3 *)     
let rec tree_map f (Tree tr) = 
    let update_elements e = 
        match e with
        | Num n -> Num (f n)
        | Sub st -> Sub (tree_map f st)
    in 
        Tree (List.map update_elements tr)
    
        
(* C.1 *)        
type expr =
  | Int of int           (* constant *)
  | Var of string        (* variable *)
  | Add of expr * expr   (* expr1 + expr2 *)
  | Mul of expr * expr   (* expr1 * expr2 *)
  | Pow of expr * int    (* expr^n *)
  
let rec simplify1 expr = 
    let pow a b = 
        let rec iter a b curr = 
            if b = 0 
                then curr 
            else 
                iter a (b - 1) (a * curr) 
        in 
        iter a b 1
    in 
    match expr with 
    | Int x -> Int x
    | Var y -> Var y
    | Add (Int 0, x) -> x
    | Add (x, Int 0) -> x
    | Add (Int x, Int y) -> Int (x + y)
    | Add (x, y) -> Add ((simplify1 x), (simplify1 y))
    | Mul (Int 0, _) -> Int 0
    | Mul (_, Int 0) -> Int 0
    | Mul (Int 1, x) -> x
    | Mul (x, Int 1) -> x
    | Mul (Int x, Int y) -> Int (x * y)
    | Mul (x, y) -> Mul ((simplify1 x), (simplify1 y))
    | Pow (_, 0) -> Int 1
    | Pow (x, 1) -> x
    | Pow (Int x, y) -> Int (pow x y)
    | Pow (x, y) -> Pow (simplify1 x, y)
    
let rec simplify expr = 
    let e = simplify1 expr in
        if expr = e 
            then expr
        else 
            simplify e
            
            
(* C.2 *)	
let rec deriv expr var = 
    match expr with 
    | Int x -> Int 0
    | Var x when x = var -> Int 1
    | Var _ -> Int 0
    | Add (x, y) -> Add (deriv x var, deriv y var)
    | Mul (x, y) -> let dx = deriv x var and dy = deriv y var in 
                    Add (Mul (x, dy), Mul (dx, y))
    | Pow (x, y) -> Mul (Mul (Int y, Pow (x, y-1)), deriv x var)
        
let derivative expr var =
  let e = simplify expr in
  let d = deriv e var in
    simplify d

