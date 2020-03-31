(* A.1 *)
let rec last_sublist = function 
  | []     -> invalid_arg "last_sublist: empty list"
  | [l]    -> [l]
  | h :: t -> last_sublist t
  
(* A.2 *)
let rec reverse lst = 
  let rec iter new_lst lst = 
    match lst with  
      | []     -> new_lst 
      | h :: t -> iter (h :: new_lst) t
    in 
      iter [] lst
      
(* A.3 *)
let rec square_list = function
  | [] -> []
  | h :: t -> h * h :: square_list t

let square_list2 items = List.map (fun x -> x * x) items

(* A.4 *)
(* Louis Reasoner's first version of the square list function takes the head
 * of the input list, squares it, then cons it to the answer list. He then
 * linearly recurses on the tail of the input list. His process essentially pops
 * all values from the first list, squares them, then pushes them onto his 
 * answer list, so that it will be in the back of the final answer list. Thus, 
 * his answer list is also in the reverse order than he wants. *)
(* His new fix wont work because the arguments to the cons function are now 
 * invalid. If we have x :: y, then x must be a single element and y must be a 
 * list element of the same type as x. However, here, Louis has x as has a list 
 * and y as a single element so it will not work. Instead of ::, he should use 
 * @ *)
(*
let square_list items =
  let rec iter things answer =
    match things with
      | [] -> answer
      | h :: t -> iter t (answer @ [(h * h)])
  in iter items []
*) 
(* This new function is not efficient. For each of the values that we add, we 
 * create a new list that we must copy every value from the old list into. We do
 * this n times and each operation takes O(n) time, thus the overall time 
 * complexity is O(n^2). *)
 
(* A.5 *)
let count_negative_numbers lst = 
  let rec iter items count = 
    match items with 
      | []                -> count
      | h :: t when h < 0 -> iter t (count + 1)
      | h :: t            -> iter t count
    in 
      iter lst 0 
 
(* A.6 *)
let power_of_two_list n = 
  let rec pow a b = 
    match b with 
      | 0 -> 1
      | _ -> a * pow a (b - 1)  
  in 
    let rec iter pow_lst i = 
      match i with 
        | 0 -> pow_lst
        | _ -> iter (pow 2 (i - 1) :: pow_lst) (i - 1)
      in 
        iter [] n
        
(* A.7 *)
let prefix_sum lst = 
  let rec iter lst prefix_lst sum = 
    match lst with 
      | []     -> prefix_lst
      | h :: t -> iter t (prefix_lst @ [sum + h]) (sum + h)
    in 
      iter lst [] 0
    
(* A.8 *)
let rec deep_reverse lst = 
  let rec iter new_lst lst = 
    match lst with  
      | []     -> new_lst 
      | h :: t -> iter ((reverse h) :: new_lst) t
    in 
      iter [] lst
      
(* A.9 *)
type 'a nested_list =
  | Value of 'a
  | List of 'a nested_list list
  
let deep_reverse_nested lst = 
  let rec iter new_lst old_lst = 
    match old_lst with 
      | []           -> new_lst 
      | Value v :: t -> iter (Value v :: new_lst) t
      | List l :: t  -> iter (List (iter [] l) :: new_lst) t
    in 
      match lst with 
        | Value v -> Value v
        | List l  -> List (iter [] l)
  
(* B.1 *)
let rec quicksort lst cmp = 
  match lst with 
    | []               -> []
    | pivot :: new_lst -> let smaller, larger = List.partition (fun n -> cmp n pivot) new_lst in
                          let left_sort = quicksort smaller cmp in 
                          let right_sort = quicksort larger cmp in 
                          left_sort @ [pivot] @ right_sort
   
(* B.2 *)
(* This quicksort function is an example of generative recursion because the 
 * recursive calls are being called on new data/list that is being generated in 
 * the previous call of quicksort, it is not being called on subsets that were 
 * of the original input, which is what structural recursion is. *)
 
(* B.3 *)
(* Running Ben's version of the code results in a stack overflow/looping 
 * recursion error. This is because removing this base case creates issues when 
 * trying to merge the list back once all the elements have been split. There is
 * no handling of lists with one element so it will keep splitting such a list 
 * to another one element list and an empty list and this process will loop 
 * forever. *)

(* B.4 *)
let rec insert_in_order new_result a_list cmp =
  match a_list with
    | [] -> [new_result]
    | h :: t when cmp new_result h -> new_result :: a_list
    | h :: t ->  h :: insert_in_order new_result t cmp

let rec insertion_sort a_list cmp =
  match a_list with
    | [] -> []
    | h :: t -> insert_in_order h (insertion_sort t cmp) cmp
 
(* This function is an exmaple of structural recursion. This is because in each 
 * call to insertion_sort we recursively call insertion_sort on subsets of the 
 * input list which were actually parts of the original input list. *)
 
(* C.1 *)
let rec subsets = function
  | [] -> [[]]
  | h :: t -> let rest = subsets t in
      rest @ (List.map (fun lst -> h :: lst) rest)
      
(* This function works because we recursively call the function until we first 
 * return [[]]. Then as we build back up from the recursion, the map function we
 * use cons the current list value to all of the lists that in our ouput list, 
 * then appends that to the output list. Essentially, we are creating 
 * the permutations at level i+1 by just adding the extra value to all the 
 * permutations at level i, then appending them to the list of level i 
 * permutations. *)

(* C.2 *)
let rec accumulate op initial sequence =
  match sequence with
    | [] -> initial
    | h :: t -> op h (accumulate op initial t)

let map p sequence =
  accumulate (fun x r -> p x :: r) [] sequence

let append seq1 seq2 =
  accumulate (fun x r -> x :: r) [] (seq1 @ seq2)

let length sequence =
  accumulate (fun _ r -> r + 1) 0 sequence
 

(* C.3 *)
let rec accumulate_n op init seqs =
  match seqs with
    | [] -> failwith "empty list"
    | [] :: _ -> []   (* assume all sequences are empty *)
    | h :: t -> accumulate op init (List.map List.hd seqs) :: 
                accumulate_n op init (List.map List.tl seqs)
    
(* C.4 *)
let rec map2 f x y =
  match (x, y) with
    | ([], [])-> []
    | ([], _) -> failwith "unequal lists"
    | (_, []) -> failwith "unequal lists"
    | (h::t, hd::tl)  -> [f h hd] @ map2 f t tl

(* dot_product v w
 * returns: the number d, where d = sum_i (v_i * w_i) *)   
let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)


(* matrix_times_vector m v
 * returns: the vector t, where t_i = sum_j (m_ij * v_j) *)
let matrix_times_vector m v = map (dot_product v) m


(* transpose m
 * returns: the matrix n, where n_ij = m_ji *)
let transpose mat = accumulate_n (fun x l -> x :: l) [] mat


(* matrix_times_matrix m n
 * returns: the matrix p, where p_ij = sum_k (m_ik * n_kj) *)
let matrix_times_matrix m n =
  let cols = transpose n in
     map (matrix_times_vector cols) m    
     
