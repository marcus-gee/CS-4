(* A.1 *)
let closest_power m n = 
    assert (m > 1 && m < n);
    
    let rec iter m n p = 
        if m * p > n 
        then p
        else iter m n (m * p) in 
    iter m n 1

(* The time complexity of closest_power is theta(log_m (n)). This is because in
 * each iteration of the function we multiply by a factor of m, so we are 
 * decreasing our search distance from n by a factor m with each iteration, thus
 * the runtime is theta(log_m (n)).
 *)

(* A.2 *)
let rec is_prefix lst1 lst2 = 
    match (lst1, lst2) with 
    | ([], lst2) -> true
    | (h1 :: t1, h2 :: t2) when h1 = h2 -> is_prefix t1 t2
    | (_, _) -> false

let rec is_subsequence lst1 lst2 = 
   match lst2 with
   | [] -> is_prefix lst1 lst2
   | (h :: t) -> 
       if is_prefix lst1 lst2 
       then true 
       else is_subsequence lst1 t
 
(* The time complexity of is_prefix is theta(n), where n is the length of the 
 * input lst1. This is because (in the worst-case scenario) we do not reach
 * any stopping conditions that return false (until possibly the last element),
 * and we search through all of lst1 to determine if it is a prefix of lst2. 
 *)     
(* The time complexity of is_subsquence is theta(n * m), where n is the length
 * of the input list lst1 and m is the length of lst2. This is because, our 
 * function is_subsequence is linear in the length of lst2 becuase we do a 
 * check for each element in lst2 to see if lst1 is a prefix of lst2 starting
 * at that element, which is theta(m). Then, we know our function is_prefix has
 * a runtime of theta(n), where n is the length of the input lst1, and since we
 * call is_prefix in every call to is_subsequence, the overall runtime is 
 * theta(m * n). 
 *)

(* A.3 *)
let rec is_embedded lst1 lst2 = 
    match (lst1, lst2) with 
    | ([], _) -> true
    | (h1 :: t1, h2 :: t2) -> 
        if h1 = h2 
        then is_embedded t1 t2
        else is_embedded lst1 t2
    | (_, _) -> false

(* The worst-case time complexity of is_embedded is theta(n), where n is the 
 * length of the input of lst2. We do a linear search over lst2 and if the 
 * head element in lst2 matches that of lst1, we remove the element from lst1. 
 * The fact that we move through all of lst2 and return the corresponding 
 * boolean once we've reached the end of it means our function runs in 
 * theta(n) time. 
 *)

(* A.4 *)    
let remove_first num lst =
    let rec iter lst new_lst = 
        match lst with 
        | [] -> None
        | (h :: t) -> 
            if num = h
            then Some (new_lst @ t)
            else iter t (new_lst @ [h]) in 
    iter lst [] 
               
let rec same_elements lst1 lst2 = 
    match (lst1, lst2) with
    | ([], []) -> true
    | ([], _) -> false
    | (h1 :: t1, lst2) -> 
        match (remove_first h1 lst2) with
            | None -> false
            | Some new_lst -> same_elements t1 new_lst

(* A.5 *)               
let rec member num lst = 
    match lst with 
    | [] -> false
    | (h :: t) -> 
        if num = h 
        then true
        else member num t
        
let remove_all num lst = 
    List.filter (fun x -> x <> num) lst   
    
let rec unique lst = 
    match lst with
    | [] -> []
    | (h :: t) -> 
        if member h t
        then (unique (remove_all h t))
        else h :: (unique t)
    
(* The worst-case time complexity of unique is theta(n^2). We have this since
 * the process of unique itself is theta(n) since unique is a tail recursive 
 * function. Then in the worst-case situation, there are no repeats in the 
 * input list, so the conditional never evaluates to true, however, each 
 * evaluation of the conditional is_member is theta(n) since it checks all 
 * values in the list to see if the input val is in it. Thus, our function 
 * calls a function which is theta(n) n times (once for each element), so we 
 * conclude the function is theta(n^2), where n is the length of the input list 
 *)   
    
(* A.6 *)
let rec intercalate nested_lst = 
    if (List.filter (fun x -> x <> []) nested_lst) = []
    then []
    else 
      (List.map List.hd (List.filter (fun x -> x <> []) nested_lst)) @ 
      intercalate (List.map List.tl (List.filter (fun x -> x <> []) nested_lst))
    
(* A.7 *)
let repeated num lst =
    if member num lst
    then match (remove_first num lst) with
         | None -> false
         | Some new_lst -> member num new_lst  
    else false     
         
let rec any bool_func lst = 
    match lst with 
    | [] -> false
    | (h :: t) -> 
        if bool_func h 
        then true
        else any bool_func t
                  
let rec all bool_func lst = 
    match lst with 
    | [] -> true
    | (h :: t) -> 
        if bool_func h 
        then all bool_func t
        else false
    
let any_repeated lst = 
    any (fun x -> repeated x lst) lst   
    
let all_repeated lst = 
    all (fun x -> repeated x lst) lst      

(* A.8 *)
let id x = x

let compose f g = 
  fun x -> f (g x)

let rec compose_all func_lst = 
    match func_lst with 
    | [] -> fun x -> x
    | (h :: t) -> compose h (compose_all t)
    
let compose_all2 func_lst = 
    List.fold_right compose func_lst id
   
(* The of compose_all and compose_all2 type signatures have to be 
 * ('a -> 'a) list -> ('a -> 'a) becuase the list of functions have to contain
 * functions that take an input of a certain type and ouput a value of the 
 * same type becuase if it outputted values of a different type, this value 
 * would be the input to the next function and it would be an invalid input.
 *) 
 
    
(* A.9 *)
let curry f = fun x y -> f (x, y)
let uncurry f = fun (x, y) -> f x y
let curry_list f = fun h t -> f (h :: t)
let uncurry_list f = fun lst -> match lst with 
                                | [] -> invalid_arg "uncurr_list"
                                | (h :: t) -> f h t
                                

(* B *) 
(* Implementation of weight-balanced trees in OCaml. *)

let tree_debug = ref true

(* Parameters.
 * - delta is used to decide if any rotation needs to be made at all
 * - gamma is used to decide between a single and a double rotation
 *)
let delta = 3
let gamma = 2

type tree =
  | Leaf
  | Node of int * int * tree * tree  (* size, value, left/right subtrees *)

let get_size = function
  | Leaf -> 0
  | Node (s, _, _, _) -> s

let empty = Leaf

let singleton v = Node (1, v, Leaf, Leaf)

let node v l r =
  let w = get_size l + get_size r + 1 in
    Node (w, v, l, r)

(* Are two subtrees balanced with respect to each other
 * i.e. "relatively balanced"?
 * Criterion:
 *   The "weight" of subtree b is not more than delta times
 *   the "weight" of subtree a.  "weight" is just the size + 1.
 *)
let rel_balanced a b =
  let wa = get_size a + 1 in
  let wb = get_size b + 1 in
    delta * wa >= wb

(* Is a single rotation indicated?
 * Criterion:
 *   The weight of subtree a is less than gamma times
 *   the weight of subtree b.
 *)
let need_single_rot a b =
  let wa = get_size a + 1 in
  let wb = get_size b + 1 in
    wa < gamma * wb

(* Find the minimum element in a tree.
 * This assumes that the tree is ordered. *)
let rec min_tree = function
  | Leaf -> None
  | Node (_, i, l, _) ->
    begin
      match min_tree l with
        | None -> Some i
        | Some l' -> Some l'
    end

(* Find the maximum element in a tree.
 * This assumes that the tree is ordered. *)
let rec max_tree = function
  | Leaf -> None
  | Node (_, i, _, r) ->
    begin
      match max_tree r with
        | None -> Some i
        | Some r' -> Some r'
    end

(* Is a tree ordered? *)
let rec ordered = function
  | Leaf -> true
  | Node (_, i, l, r) ->
    begin
      ordered l &&
      ordered r &&
      match (max_tree l, min_tree r) with
        | (None, None) -> true
        | (Some l', None) -> l' < i
        | (None, Some r') -> i < r'
        | (Some l', Some r') -> l' < i && i < r'
    end

(* Is a tree balanced with respect to the weight criteria? *)
let rec balanced = function
  | Leaf -> true
  | Node (_, _, l, r) ->
      rel_balanced l r &&
      rel_balanced r l &&
      balanced l &&
      balanced r

(* ----------------------------------------------------------------------
 * Tree rotations and rebalancing.
 * ---------------------------------------------------------------------- *)
(* B.2 *)
let single_l v l r =
  if !tree_debug then
    Printf.printf "- single_l at tree rooted at %d\n" v;
  match r with
    | Leaf -> invalid_arg "single_l"
    | Node (sz, v1, rl, rr) ->
         Node (sz + (get_size l) + 1, v1, 
         Node ((get_size l) + (get_size rl)+1, v, l, rl), rr)
        
(* B.4 *)
let single_r v l r =
  if !tree_debug then
    Printf.printf "- single_r at tree rooted at %d\n" v;
  match l with
    | Leaf -> invalid_arg "single_r"
    | Node (sz, v1, ll, lr) ->
        Node (sz + (get_size r) + 1, v1, ll, 
        Node ((get_size lr) + (get_size r) + 1, v, lr, r))  
          
(* B.3 *)
(* can get a double_r rotation by performing a single_r from the right node 
 * then a single left from the root node. *)
let double_l v l r =
  if !tree_debug then
    Printf.printf "- double_l at tree rooted at %d\n" v;
  match r with
    | Leaf
    | Node (_, _, Leaf, _) -> invalid_arg "double_l"
    | Node (sz, v1, rl, rr) ->
        single_l v l (single_r v1 rl rr)    
       
(* B.5 *)
let double_r v l r =
  if !tree_debug then
    Printf.printf "- double_r at tree rooted at %d\n" v;
  match l with
    | Leaf
    | Node (_, _, _, Leaf) -> invalid_arg "double_r"
    | Node (sz, v1, ll, lr) -> 
        single_r v (single_l v1 ll lr) r

let rotate_l v l r =
  if !tree_debug then
    Printf.printf "rotate_l at tree rooted at %d\n" v;
  match r with
    | Leaf -> invalid_arg "rotate_l"
    | Node (_, _, rl, rr) ->
      if need_single_rot rl rr then
        single_l v l r
      else
        double_l v l r     

let rotate_r v l r =
  if !tree_debug then
    Printf.printf "rotate_r at tree rooted at %d\n" v;
  match l with
    | Leaf -> invalid_arg "rotate_r"
    | Node (_, _, ll, lr) ->
      if need_single_rot lr ll then
        single_r v l r
      else
        double_r v l r

(* ----------------------------------------------------------------------
 * Membership testing.
 * ---------------------------------------------------------------------- *)

(* B.1 *)
(* Is a value a member of a tree? *)
let rec tree_member v t = 
    match t with 
    | Leaf -> false
    | Node (_, data, l, r) -> 
        if v < data 
        then tree_member v l 
        else if v > data
        then tree_member v r
        else true
    

(* ----------------------------------------------------------------------
 * Inserting values into trees.
 * ---------------------------------------------------------------------- *)

let balance_l v l r =
  if rel_balanced l r then
    node v l r
  else
    rotate_l v l r

let balance_r v l r =
  if rel_balanced r l then
    node v l r
  else
    rotate_r v l r

(* Insert a value into a tree with rebalancing. *)
let rec insert v t =
  match t with
    | Leaf -> singleton v
    | Node (_, v', _, _) when v = v' -> t
    | Node (_, v', l, r) ->
        if v < v' then
          (* Adding to the left means you may have to rotate to the right. *)
          balance_r v' (insert v l) r
        else
          (* Adding to the right means you may have to rotate to the left. *)
          balance_l v' l (insert v r)   
    
(* TEST CODE *)
(* Convert a list into a tree *)
let tree_of_list lst =
  List.fold_left (fun t x -> insert x t) empty lst

(* Seed the random number generator. *)
let _ = Random.self_init ()

(* Generate a list of random integers.
 * Pick `n` random ints between `0` and `imax - 1`. *)
let random_list n imax =
  let rec iter n lst =
    if n = 0 then
      lst
    else
      iter (n - 1) (Random.int imax :: lst)
  in
    iter n []

(* Generate a tree of random integers.
 * Pick `n` random ints between `0` and `imax - 1`
 * and add them to the tree one after another. *)
let random_tree n imax = tree_of_list (random_list n imax)

(* Pretty-print a tree. *)
let print_tree tree =
  let blanks n = String.make n ' ' in
  let rec aux tree indent =
    let ind = blanks indent in
      match tree with
        | Leaf -> Printf.printf "%sLeaf\n" ind
        | Node (d, v, l, r) ->
          begin
            Printf.printf "%sNode[(%d) [size %d]\n" ind v d;
            aux l (indent + 2);
            Printf.printf "%s  ----\n" ind;
            aux r (indent + 2);
            Printf.printf "%s]\n" ind;
          end
  in
    aux tree 0    
(* TEST CODE *)    

