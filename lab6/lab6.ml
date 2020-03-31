exception Stat_error of string

(* A.1 *)
(*
    DESUGAR EXPRESSION TO ->
    let factorial = 
        fun n -> let rec iter m r = 
                    if m = 0
                    then r 
                    else iter (m - 1) (r * m)
                 in iter n 1
        in 
            factorial 3
                     

    FRAME 0 (initial environment)
        parent: none
        bindings:
            - : [primitive function -]
            * : [primitive function *]
            
    FUNCTION 0 (fun n -> let rec iter m r = ... )
        env: FRAME 0
        param: n
        body: (let rec iter m r = ... )
        
    FRAME 1 (let factorial = FUNCTION 0 in ... )
        parent: FRAME 0 
        bindings:
            factorial: FUNCTION 0
            
    FRAME 2 (FUNCTION 0 applied to 3)
        parent: FRAME 0
        bindings: 
            n: 3
            
    FUNCTION 1 (fun m r -> if m = 0 then 1 else ... )
        env: FRAME 3
        parent: m r
        body: (if m = 0 then 1 else ... )
        
    FRAME 3 (let rec iter = FUNCTION 1 in ... )
        parent: FRAME 2 
        bindings 
            iter: FUNCTION 1
            
    FRAME 4 (FUNCTION 1 applied to 3 1)
        parent: FRAME 3
        bindings: 
            m: 3
            r: 1
        
    FRAME 5 (FUNCTION 1 applied to 2 3)
        parent: FRAME 3
        bindings:
            m: 2 
            r: 3
            
    FRAME 6 (FUNCTION 1 applied to 1 6)
        parent: FRAME 3
        bindings:
            m: 1 
            r: 6
            
    FRAME 7 (FUNCTION 1 applied to 0 6)
        parent: FRAME 3
        bindings:
            m: 0 
            r: 6
*)

(* A.2 *)
let factorial = 
    let f = ref (fun n -> 0) in
    fun n -> 
        f := (fun m -> if m = 0 then 1 
                       else m * !f(m - 1));
    !f n

    
        
(* B.1 *)
let make_stat_1 () = 
    let sum   = ref 0.0 and 
        sumsq = ref 0.0 and
        n     = ref 0.0 in
    object 
        method append data = 
            sum   := !sum +. data;
            sumsq := !sumsq +. (data *. data);
            n     := !n +. 1.0    
        method mean = 
            match !n with 
            | 0.0 -> raise (Stat_error "need at least one value for mean")
            | _   -> !sum /. !n
        method variance = 
            match !n with 
            | 0.0 -> raise (Stat_error "need at least one value for variance")
            | _   -> (!sumsq -. ((!sum *. !sum) /. !n)) /. !n      
        method stdev = 
            match !n with 
            | 0.0 -> raise (Stat_error "need at least one value for stdev")
            | _   -> sqrt((!sumsq -. ((!sum *. !sum) /. !n)) /. !n)  
        method clear = 
            sum   := 0.0;
            sumsq := 0.0;
            n     := 0.0
     end
     
(* B.2 *)
let make_stat_2 () = 
   let sum   = ref 0.0 and 
        sumsq = ref 0.0 and
        n     = ref 0.0 in
    object (self)
        method append data = 
            sum   := !sum +. data;
            sumsq := !sumsq +. (data *. data);
            n     := !n +. 1.0    
        method mean = 
            match !n with 
            | 0.0 -> raise (Stat_error "need at least one value for mean")
            | _   -> !sum /. !n
        method private _variance = 
            (!sumsq -. ((!sum *. !sum) /. !n)) /. !n    
        method variance = 
            match !n with 
            | 0.0 -> raise (Stat_error "need at least one value for variance")
            | _   -> self#_variance    
        method stdev = 
            match !n with 
            | 0.0 -> raise (Stat_error "need at least one value for stdev")
            | _   -> sqrt(self#_variance)  
        method clear = 
            sum   := 0.0;
            sumsq := 0.0;
            n     := 0.0
     end    


(* C.1 *)
(* Signature for priority queues. *)
module type PRIORITY_QUEUE =
  sig
    exception Empty

    type elem      (* Abstract type of elements of queue. *)
    type t         (* Abstract type of queue. *)

    val empty      : t                (* The empty queue.         *)
    val is_empty   : t -> bool        (* Check if queue is empty. *)
    val insert     : t -> elem -> t   (* Insert item into queue.  *)
    val find_min   : t -> elem        (* Return minimum element.  *)
    val delete_min : t -> t           (* Delete minimum element.  *)
    val from_list  : elem list -> t   (* Convert list to queue.   *)
  end;;
 
(* Priority Queue implementation *) 
module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
    struct
    exception Empty

    type elem = int  
    type t = Leaf | Node of elem * int * t * t 


    let empty = Leaf 
    
    let is_empty q = 
        if q = empty 
        then true 
        else false
        
    let rank node = 
        match node with 
        | Leaf -> 0
        | Node (_, r, _, _) -> r
    
    let rec merge first second = 
        let new_h min h1 h2 = 
            if rank h1 < rank h2 
            then Node (min, 1 + (rank h1), h2, h1)
            else Node (min, 1 + (rank h2), h1, h2)
        in
        match (first, second) with
        | (Leaf, s) -> s
        | (f, Leaf) -> f
        | (Node (m1, ra1, l1, r1), Node (m2, ra2, l2, r2)) -> 
            begin
                if m1 < m2
                then new_h m1 l1 (merge r1 (Node (m2, ra2, l2, r2)))
                else new_h m2 l2 (merge r2 (Node (m1, ra1, l1, r1)))
            end
               
    let insert q element = 
        merge q (Node (element, 1, Leaf, Leaf))
        
    let find_min q = 
        match q with 
        | Leaf -> raise Empty 
        | Node (m, _, _, _) -> m
        
    let delete_min q = 
        match q with 
        | Leaf -> raise Empty
        | Node (m, ra, l, r) -> merge l r
        
    let rec from_list lst = 
        match lst with 
        | [] -> Leaf
        | (h :: t) -> insert (from_list t) h
    end     
        
let heap_sort lst = 
    let rec iter q sorted =
        if PriorityQueue.is_empty q 
        then List.rev sorted
        else iter 
        (PriorityQueue.delete_min q) ((PriorityQueue.find_min q) :: sorted)
    in 
        iter (PriorityQueue.from_list lst) []
        
(* C.2 *)
(* Type for ordered comparisons. *)
type comparison = LT | EQ | GT

(* Signature for ordered objects. *)
module type ORDERED =
  sig
    type t
    val cmp: t -> t -> comparison
  end;;
    
module OrderedString =
  struct
    type t = string
    let cmp x y =
      if x = y then EQ else if x < y then LT else GT
  end
  

module MakePriorityQueue (Elt : ORDERED)
  : (PRIORITY_QUEUE with type elem = Elt.t) =
  struct
    exception Empty

    type elem = Elt.t  
    type t = Leaf | Node of elem * int * t * t 


    let empty = Leaf 
    
    let is_empty q = 
        if q = empty 
        then true 
        else false
        
    let rank node = 
        match node with 
        | Leaf -> 0
        | Node (_, r, _, _) -> r
    
    let rec merge first second = 
        let new_h min h1 h2 = 
            if rank h1 < rank h2 
            then Node (min, 1 + (rank h1), h2, h1)
            else Node (min, 1 + (rank h2), h1, h2)
        in
        match (first, second) with
        | (Leaf, s) -> s
        | (f, Leaf) -> f
        | (Node (m1, ra1, l1, r1), Node (m2, ra2, l2, r2)) -> 
            begin
                if m1 < m2
                then new_h m1 l1 (merge r1 (Node (m2, ra2, l2, r2)))
                else new_h m2 l2 (merge r2 (Node (m1, ra1, l1, r1)))
            end
               
    let insert q element = 
        merge q (Node (element, 1, Leaf, Leaf))
        
    let find_min q = 
        match q with 
        | Leaf -> raise Empty 
        | Node (m, _, _, _) -> m
        
    let delete_min q = 
        match q with 
        | Leaf -> raise Empty
        | Node (m, ra, l, r) -> merge l r
        
    let rec from_list lst = 
        match lst with 
        | [] -> Leaf
        | (h :: t) -> insert (from_list t) h
  end
 
        
module StringPQ = MakePriorityQueue(OrderedString)

let heap_sort_2 lst = 
   let rec iter q sorted =
        if StringPQ.is_empty q 
        then List.rev sorted
        else iter 
        (StringPQ.delete_min q) ((StringPQ.find_min q) :: sorted)
    in 
        iter (StringPQ.from_list lst) []     
    
        
(* D.1 *)
type 'a evaluate = Unevaluated of (unit -> 'a) | Evaluated of 'a
type 'a lazy_t = 'a evaluate ref
let make_lazy e = ref (Unevaluated e)
let force lz = 
    match !lz with
    | Unevaluated f -> let val_ = f () in 
                       begin
                           lz := Evaluated val_;
                           val_
                       end
    | Evaluated e -> e

(* D.2 *)      
let y =
  fun f ->
    (fun z -> z (`Roll z))
    (fun (`Roll w) -> f (fun x -> w (`Roll w) x))

(* a *)    
let almost_sum = 
    fun s -> 
        fun n -> 
            match n with  
            | [] -> 0
            | (h :: t) -> h + (s t)

let sum = y almost_sum 
    
(* b *)       
let factorial2 n = 
    let iter = 
        fun f -> 
            fun a -> 
                match a with 
                | (0, r) -> r
                | (n, r) -> f (n - 1, n * r)
    in 
        y iter (n, 1)
