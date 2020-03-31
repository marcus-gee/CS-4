(* CS 4, Winter 2020. Interface file for the midterm exam. *)

(* Part A. *)

val closest_power  : int -> int -> int
val is_prefix      : int list -> int list -> bool
val is_subsequence : int list -> int list -> bool
val is_embedded    : int list -> int list -> bool
val remove_first   : int -> int list -> int list option
val same_elements  : int list -> int list -> bool
val member         : int -> int list -> bool
val remove_all     : int -> int list -> int list
val unique         : int list -> int list
val intercalate    : int list list -> int list
val any            : ('a -> bool) -> 'a list -> bool
val repeated       : int -> int list -> bool
val all            : ('a -> bool) -> 'a list -> bool
val any_repeated   : int list -> bool
val all_repeated   : int list -> bool
val compose_all    : ('a -> 'a) list -> ('a -> 'a)
val compose_all2   : ('a -> 'a) list -> ('a -> 'a)
val curry          : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry        : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val curry_list     : ('a list -> 'b) -> 'a -> 'a list -> 'b
val uncurry_list   : ('a -> 'a list -> 'b) -> 'a list -> 'b

(* Part B. *)
val tree_debug : bool ref
val delta      : int
val gamma      : int

type tree = 
  | Leaf 
  | Node of int * int * tree * tree

val get_size        : tree -> int
val empty           : tree
val singleton       : int -> tree
val node            : int -> tree -> tree -> tree
val rel_balanced    : tree -> tree -> bool
val need_single_rot : tree -> tree -> bool
val min_tree        : tree -> int option
val max_tree        : tree -> int option
val ordered         : tree -> bool
val balanced        : tree -> bool
val single_l        : int -> tree -> tree -> tree
val double_l        : int -> tree -> tree -> tree
val rotate_l        : int -> tree -> tree -> tree
val single_r        : int -> tree -> tree -> tree
val double_r        : int -> tree -> tree -> tree
val rotate_r        : int -> tree -> tree -> tree
val balance_l       : int -> tree -> tree -> tree
val balance_r       : int -> tree -> tree -> tree
val tree_member     : int -> tree -> bool
val insert          : int -> tree -> tree
val tree_of_list    : int list -> tree
val random_list     : int -> int -> int list
val random_tree     : int -> int -> tree
val print_tree      : tree -> unit

