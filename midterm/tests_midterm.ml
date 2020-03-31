open OUnit2
open Midterm

(* 
 * Utilities. 
 *)

let assert_false msg x = assert_bool msg (not x)
let assert_true msg x = assert_bool msg x
let assert_not_equal msg x y = assert_bool msg (not (x = y))

let assert_raises_failure msg x =
  assert_bool msg
    (try
       begin
          ignore (x ());
          false
       end
     with
       | Failure _ -> true
       | _ -> false)

let assert_raises_invalid_arg msg x =
  assert_bool msg
    (try
       begin
          ignore (x ());
          false
       end
     with
       | Invalid_argument _ -> true
       | _ -> false)

(* Print a list of integers. *)
let print_int_list lst =
  let rec aux lst =
    match lst with
      | [] -> ""
      | h :: t -> ";" ^ string_of_int h ^ aux t
    in
  let s = 
    match lst with
      | [] -> "[]"
      | h :: t -> "[" ^ string_of_int h ^ aux t ^ "]"
  in
    Printf.printf "%s\n" s

(*
 * Sample trees for part B.
 *)

let _ = tree_debug := false

let t1  = empty
let t2  = insert  2 t1
let t3  = insert 66 t2
let t4  = insert 36 t3
let t5  = insert 85 t4
let t6  = insert 96 t5
let t7  = insert  7 t6
let t8  = insert 26 t7
let t9  = insert 36 t8
let t10 = insert 12 t9
let t11 = insert 62 t10
let t12 = insert 38 t11
let t13 = insert 14 t12
let t14 = insert 23 t13
let t15 = insert 16 t14
let t16 = insert 38 t15
let t17 = insert 65 t16
let t18 = insert 91 t17
let t19 = insert 27 t18

let t1_correct = Leaf

let t2_correct = Node (1, 2, Leaf, Leaf)

let t3_correct = 
  Node (2, 2, Leaf, Node (1, 66, Leaf, Leaf))

let t4_correct = 
  Node (3, 2, Leaf, Node (2, 66, Node (1, 36, Leaf, Leaf), Leaf))

let t5_correct = 
  Node (4, 66, Node (2, 2, Leaf, Node (1, 36, Leaf, Leaf)),
   Node (1, 85, Leaf, Leaf))

let t6_correct = 
  Node (5, 66, Node (2, 2, Leaf, Node (1, 36, Leaf, Leaf)),
   Node (2, 85, Leaf, Node (1, 96, Leaf, Leaf)))

let t7_correct = 
  Node (6, 66, Node (3, 2, Leaf, Node (2, 36, Node (1, 7, Leaf, Leaf), Leaf)),
   Node (2, 85, Leaf, Node (1, 96, Leaf, Leaf)))

let t8_correct = 
  Node (7, 66,
   Node (4, 7, Node (1, 2, Leaf, Leaf),
    Node (2, 36, Node (1, 26, Leaf, Leaf), Leaf)),
   Node (2, 85, Leaf, Node (1, 96, Leaf, Leaf)))

let t9_correct = 
  Node (7, 66,
   Node (4, 7, Node (1, 2, Leaf, Leaf),
    Node (2, 36, Node (1, 26, Leaf, Leaf), Leaf)),
   Node (2, 85, Leaf, Node (1, 96, Leaf, Leaf)))

let t10_correct = 
  Node (8, 66,
   Node (5, 7, Node (1, 2, Leaf, Leaf),
    Node (3, 36, Node (2, 26, Node (1, 12, Leaf, Leaf), Leaf), Leaf)),
   Node (2, 85, Leaf, Node (1, 96, Leaf, Leaf)))

let t11_correct = 
  Node (9, 66,
   Node (6, 7, Node (1, 2, Leaf, Leaf),
    Node (4, 36, Node (2, 26, Node (1, 12, Leaf, Leaf), Leaf),
     Node (1, 62, Leaf, Leaf))),
   Node (2, 85, Leaf, Node (1, 96, Leaf, Leaf)))

let t12_correct = 
  Node (10, 66,
   Node (7, 7, Node (1, 2, Leaf, Leaf),
    Node (5, 36, Node (2, 26, Node (1, 12, Leaf, Leaf), Leaf),
     Node (2, 62, Node (1, 38, Leaf, Leaf), Leaf))),
   Node (2, 85, Leaf, Node (1, 96, Leaf, Leaf))) 

let t13_correct = 
  Node (11, 66,
   Node (8, 36,
    Node (5, 7, Node (1, 2, Leaf, Leaf),
     Node (3, 26, Node (2, 12, Leaf, Node (1, 14, Leaf, Leaf)), Leaf)),
    Node (2, 62, Node (1, 38, Leaf, Leaf), Leaf)),
   Node (2, 85, Leaf, Node (1, 96, Leaf, Leaf)))

let t14_correct = 
  Node (12, 36,
   Node (6, 7, Node (1, 2, Leaf, Leaf),
    Node (4, 14, Node (1, 12, Leaf, Leaf),
     Node (2, 26, Node (1, 23, Leaf, Leaf), Leaf))),
   Node (5, 66, Node (2, 62, Node (1, 38, Leaf, Leaf), Leaf),
    Node (2, 85, Leaf, Node (1, 96, Leaf, Leaf))))

let t15_correct = 
  Node (13, 36,
   Node (7, 7, Node (1, 2, Leaf, Leaf),
    Node (5, 14, Node (1, 12, Leaf, Leaf),
     Node (3, 26, Node (2, 23, Node (1, 16, Leaf, Leaf), Leaf), Leaf))),
   Node (5, 66, Node (2, 62, Node (1, 38, Leaf, Leaf), Leaf),
    Node (2, 85, Leaf, Node (1, 96, Leaf, Leaf))))

let t16_correct = 
  Node (13, 36,
   Node (7, 7, Node (1, 2, Leaf, Leaf),
    Node (5, 14, Node (1, 12, Leaf, Leaf),
     Node (3, 26, Node (2, 23, Node (1, 16, Leaf, Leaf), Leaf), Leaf))),
   Node (5, 66, Node (2, 62, Node (1, 38, Leaf, Leaf), Leaf),
    Node (2, 85, Leaf, Node (1, 96, Leaf, Leaf))))

let t17_correct = 
  Node (14, 36,
   Node (7, 7, Node (1, 2, Leaf, Leaf),
    Node (5, 14, Node (1, 12, Leaf, Leaf),
     Node (3, 26, Node (2, 23, Node (1, 16, Leaf, Leaf), Leaf), Leaf))),
   Node (6, 66,
    Node (3, 62, Node (1, 38, Leaf, Leaf), Node (1, 65, Leaf, Leaf)),
    Node (2, 85, Leaf, Node (1, 96, Leaf, Leaf))))

let t18_correct = 
  Node (15, 36,
   Node (7, 7, Node (1, 2, Leaf, Leaf),
    Node (5, 14, Node (1, 12, Leaf, Leaf),
     Node (3, 26, Node (2, 23, Node (1, 16, Leaf, Leaf), Leaf), Leaf))),
   Node (7, 66,
    Node (3, 62, Node (1, 38, Leaf, Leaf), Node (1, 65, Leaf, Leaf)),
    Node (3, 85, Leaf, Node (2, 96, Node (1, 91, Leaf, Leaf), Leaf))))

let t19_correct = 
  Node (16, 36,
   Node (8, 14, Node (3, 7, Node (1, 2, Leaf, Leaf), Node (1, 12, Leaf, Leaf)),
    Node (4, 26, Node (2, 23, Node (1, 16, Leaf, Leaf), Leaf),
     Node (1, 27, Leaf, Leaf))),
   Node (7, 66,
    Node (3, 62, Node (1, 38, Leaf, Leaf), Node (1, 65, Leaf, Leaf)),
    Node (3, 85, Leaf, Node (2, 96, Node (1, 91, Leaf, Leaf), Leaf))))

(*
 * The tests.
 *)

let all_tests = "all" >:::
[ 

  (* ---------------------------------------------------------------------- 
   * Part A.
   * ---------------------------------------------------------------------- *)

  "Problem A.1: closest_power" >:: (fun _ ->
    assert_equal ~msg:"closest_power 1"  (closest_power 2 4) 4;
    assert_equal ~msg:"closest_power 2"  (closest_power 2 5) 4;
    assert_equal ~msg:"closest_power 3"  (closest_power 2 1000) 512;
    assert_equal ~msg:"closest_power 4"  (closest_power 2 65535) 32768;
    assert_equal ~msg:"closest_power 5"  (closest_power 2 65536) 65536;
    assert_equal ~msg:"closest_power 6"  (closest_power 2 1000000) 524288;
    assert_equal ~msg:"closest_power 7"  (closest_power 3 4) 3;
    assert_equal ~msg:"closest_power 8"  (closest_power 3 56) 27;
    assert_equal ~msg:"closest_power 9"  (closest_power 3 191) 81;
    assert_equal ~msg:"closest_power 10" (closest_power 3 65536) 59049;
    assert_equal ~msg:"closest_power 11" (closest_power 16 65535) 4096;
    assert_equal ~msg:"closest_power 12" (closest_power 16 65536) 65536;
    assert_equal ~msg:"closest_power 13" (closest_power 16 65537) 65536;
    assert_equal ~msg:"closest_power 14" (closest_power 100 65537) 10000;
  );

  "Problem A.2: is_prefix, is_subsequence" >:: (fun _ ->
    assert_true  "is_prefix 1"  (is_prefix [] []);
    assert_true  "is_prefix 2"  (is_prefix [] [1;2;3;4;5]);
    assert_false "is_prefix 3"  (is_prefix [1] []);
    assert_true  "is_prefix 4"  (is_prefix [1] [1]);
    assert_false "is_prefix 5"  (is_prefix [1] [2]);
    assert_false "is_prefix 6"  (is_prefix [1;2;3] [1]);
    assert_false "is_prefix 7"  (is_prefix [1;2;3] [1;2]);
    assert_true  "is_prefix 8"  (is_prefix [1;2;3] [1;2;3]);
    assert_true  "is_prefix 9"  (is_prefix [1;2;3] [1;2;3;4;5]);
    assert_false "is_prefix 10" (is_prefix [1;2;3] [0;1;2;3;4;5]);
    assert_false "is_prefix 11" (is_prefix [5;1;4] [0;1;5;1;4;5]);

    assert_true  "is_subsequence 1" (is_subsequence [] []);
    assert_true  "is_subsequence 2" (is_subsequence [] [1;2;3;4;5]);
    assert_false "is_subsequence 3" (is_subsequence [1] []);
    assert_true  "is_subsequence 4" (is_subsequence [1] [1]);
    assert_true  "is_subsequence 5" (is_subsequence [1] [5;4;3;2;1]);
    assert_true  "is_subsequence 6" (is_subsequence [1;2;3] [1;2;3;5;4;3;2;1]);
    assert_true  "is_subsequence 7" (is_subsequence [1;2;3] [1;2;1;2;3;2;1]);
  );

  "Problem A.3: is_embedded" >:: (fun _ ->
    assert_true  "is_embedded 1" (is_embedded [] []);
    assert_true  "is_embedded 2" (is_embedded [] [1;2;3]);
    assert_false "is_embedded 3" (is_embedded [1] []);
    assert_true  "is_embedded 4" (is_embedded [1;2;3] [1;2;3;4]);
    assert_true  "is_embedded 5" (is_embedded [1;2;3] [0;1;0;2;0;3;0;4;0]);
    assert_true  "is_embedded 6" (is_embedded [1;2;3;4] [0;1;4;3;2;4;3;0;4;0]);
    assert_false "is_embedded 7" (is_embedded [1;2;3] [3;0;1;0;2;0;4]);
    assert_true  "is_embedded 8" (is_embedded [1;2;3] [3;0;1;0;2;0;4;3]);
    assert_true  "is_embedded 9" (is_embedded [4;3;3;5] [1;2;3;4;3;5;3;5]);
  );

  "Problem A.4: remove_first, same_elements" >:: (fun _ ->
    assert_equal ~msg:"remove_first 1"  (remove_first 1 []) None;
    assert_equal ~msg:"remove_first 2"  (remove_first 1 [1]) (Some []);
    assert_equal ~msg:"remove_first 3"  (remove_first 1 [2]) None;
    assert_equal ~msg:"remove_first 4"  
      (remove_first 1 [1;1;1;1;1]) (Some [1;1;1;1]);
    assert_equal ~msg:"remove_first 5"  
      (remove_first 1 [1;2;3;4;5]) (Some [2;3;4;5]);
    assert_equal ~msg:"remove_first 6"  
      (remove_first 1 [5;4;3;2;1;2;3;4;5]) (Some [5;4;3;2;2;3;4;5]);
    assert_equal ~msg:"remove_first 7"  
      (remove_first 1 [5;4;3;2;1;2;1;3;1;4;1;5]) (Some [5;4;3;2;2;1;3;1;4;1;5]);

    assert_true  "same_elements 1"  (same_elements [] []);
    assert_false "same_elements 2"  (same_elements [1] []);
    assert_false "same_elements 3"  (same_elements [] [1]);
    assert_false "same_elements 4"  (same_elements [1;2] [1;2;3]);
    assert_false "same_elements 5"  (same_elements [1;2;3] [1;2]);
    assert_true  "same_elements 6"  (same_elements [1;2;3] [1;2;3]);
    assert_true  "same_elements 7"  (same_elements [1;2;3] [3;2;1]);
    assert_false "same_elements 8"  (same_elements [1;2;3] [3;2;3]);
    assert_false "same_elements 9"  (same_elements [1;2;3] [3;2;1;2]);
    assert_false "same_elements 10" (same_elements [1;2;3;3] [3;1;1;2]);
    assert_true  "same_elements 11" (same_elements [1;2;3;3] [3;1;3;2]);
  );

  "Problem A.5: member, remove_all, unique" >:: (fun _ ->
    assert_false "member 1" (member 1 []); 
    assert_false "member 2" (member 1 [0]); 
    assert_true  "member 3" (member 1 [1]); 
    assert_true  "member 4" (member 1 [1;1;1]); 
    assert_true  "member 5" (member 1 [1;2;3]); 
    assert_true  "member 6" (member 1 [3;2;1]); 
    assert_true  "member 7" (member 1 [3;1;2;1]); 
    assert_false "member 8" (member 0 [3;1;2;1]); 

    assert_equal ~msg:"remove_all 1" (remove_all 1 []) [];
    assert_equal ~msg:"remove_all 2" (remove_all 1 [1]) [];
    assert_equal ~msg:"remove_all 3" (remove_all 1 [1;1;1]) [];
    assert_equal ~msg:"remove_all 4" (remove_all 1 [1;2;3]) [2;3];
    assert_equal ~msg:"remove_all 5" 
      (remove_all 1 [1;2;3;1;2;3;1;2;3]) [2;3;2;3;2;3];

    assert_equal ~msg:"unique 1" (unique []) [];
    assert_equal ~msg:"unique 2" (unique [1;1;1]) [];
    assert_equal ~msg:"unique 3" (unique [1;2;1]) [2];
    assert_equal ~msg:"unique 4" (unique [1;2;1;3;4;2;1;5;3;6]) [4;5;6];
  );

  "Problem A.6: intercalate" >:: (fun _ ->
    assert_equal ~msg:"intercalate 1" (intercalate []) [];
    assert_equal ~msg:"intercalate 2" (intercalate [[]]) [];
    assert_equal ~msg:"intercalate 3" (intercalate [[];[];[]]) [];
    assert_equal ~msg:"intercalate 4" 
      (intercalate [[1;2;3];[4;5;6];[7;8;9]]) [1;4;7;2;5;8;3;6;9];
    assert_equal ~msg:"intercalate 5" 
      (intercalate [[1];[2;3];[4;5;6]]) [1;2;4;3;5;6];
    assert_equal ~msg:"intercalate 6" 
      (intercalate [[1;1;1];[2;3];[4;5;6]]) [1;2;4;1;3;5;1;6];
    assert_equal ~msg:"intercalate 7" 
      (intercalate [[1;1;1];[];[4;5;6];[2;2]]) [1;4;2;1;5;2;1;6];
  );

  "Problem A.7: any, all, repeated" >:: (fun _ ->
    assert_false "repeated 1" (repeated 1 []);
    assert_false "repeated 2" (repeated 1 [1]);
    assert_true  "repeated 3" (repeated 1 [1;1]);
    assert_true  "repeated 4" (repeated 1 [1;1;1]);
    assert_true  "repeated 5" (repeated 1 [1;0;1]);
    assert_false "repeated 6" (repeated 1 [0;1;0;0;2;0]);
    assert_true  "repeated 7" (repeated 1 [0;1;0;0;1;0]);

    let zeros x = x = 0 in
      begin
        assert_false "any 1" (any zeros []);
        assert_false "any 2" (any zeros [1]);
        assert_false "any 3" (any zeros [1;2;3]);
        assert_true  "any 4" (any zeros [0]);
        assert_true  "any 5" (any zeros [0;0]);
        assert_true  "any 6" (any zeros [0;0;0]);
        assert_true  "any 7" (any zeros [0;1;2;3]);
        assert_true  "any 8" (any zeros [0;1;0;2;0;3]);

        assert_true  "all 1" (all zeros []);
        assert_false "all 2" (all zeros [1]);
        assert_false "all 3" (all zeros [1;2;3]);
        assert_true  "all 4" (all zeros [0]);
        assert_true  "all 5" (all zeros [0;0]);
        assert_true  "all 6" (all zeros [0;0;0]);
        assert_false "all 7" (all zeros [0;1;2;3]);
        assert_false "all 8" (all zeros [0;1;0;2;0;3]);
      end;
                               
      assert_false "any_repeated 1" (any_repeated []);
      assert_false "any_repeated 2" (any_repeated [1]);
      assert_false "any_repeated 3" (any_repeated [1;2;3]);
      assert_true  "any_repeated 4" (any_repeated [1;2;3;1]);
      assert_true  "any_repeated 5" (any_repeated [1;1;1;1]);
      assert_true  "any_repeated 6" (any_repeated [0;2;3;1;4;6;2;4]);
      assert_true  "any_repeated 7" (all_repeated [2;1;3;1;4;1;2;1;3;2;4;1]);

      assert_true  "all_repeated 1" (all_repeated []);
      assert_false "all_repeated 2" (all_repeated [1]);
      assert_false "all_repeated 3" (all_repeated [1;2;3]);
      assert_false "all_repeated 4" (all_repeated [1;2;3;1]);
      assert_true  "all_repeated 5" (all_repeated [1;1;1;1]);
      assert_false "all_repeated 6" (all_repeated [0;2;3;1;4;6;2;4]);
      assert_true  "all_repeated 7" (all_repeated [2;1;3;1;4;1;2;1;3;2;4;1]);
  );

  "Problem A.8: compose_all" >:: (fun _ ->
    let f1 x = x + 1 in
    let f2 x = x * 2 in
    let f3 x = x - 3 in
    let g1 = compose_all [f1; f2; f3] in
    let g2 = compose_all2 [f1; f2; f3] in
      begin
        assert_equal ~msg:"compose_all 1" (g1 10) (f1 (f2 (f3 10)));
        assert_equal ~msg:"compose_all 2" (g2 10) (f1 (f2 (f3 10)));
      end
  );

  (* ---------------------------------------------------------------------- 
   * Part B.
   * ---------------------------------------------------------------------- *)

  "Part B: member" >:: (fun _ ->
    assert_false "tree_member 1" (tree_member  1 t1);

    assert_true  "tree_member 2" (tree_member  2 t2);
    assert_false "tree_member 3" (tree_member  1 t1);

    assert_true  "tree_member 4" (tree_member 66 t3);
    assert_false "tree_member 5" (tree_member 27 t3);

    assert_true  "tree_member 6" (tree_member 36 t4);
    assert_false "tree_member 7" (tree_member 91 t4);

    assert_true  "tree_member 8" (tree_member 85 t5);
    assert_false "tree_member 9" (tree_member 65 t5);

    assert_true  "tree_member 10" (tree_member 96 t6);
    assert_false "tree_member 11" (tree_member 38 t6);

    assert_true  "tree_member 12" (tree_member  7 t7);
    assert_false "tree_member 13" (tree_member 16 t7);

    assert_true  "tree_member 14" (tree_member 26 t8);
    assert_false "tree_member 15" (tree_member 23 t8);

    assert_true  "tree_member 16" (tree_member 36 t9);
    assert_false "tree_member 17" (tree_member 14 t9);

    assert_true  "tree_member 18" (tree_member 12 t10);
    assert_false "tree_member 19" (tree_member 38 t10);

    assert_true  "tree_member 20" (tree_member 62 t11);
    assert_false "tree_member 21" (tree_member 27 t11);

    assert_true  "tree_member 22" (tree_member 38 t12);
    assert_false "tree_member 23" (tree_member 91 t12);

    assert_true  "tree_member 24" (tree_member 14 t13);
    assert_false "tree_member 25" (tree_member 65 t13);

    assert_true  "tree_member 26" (tree_member 23 t14);
    assert_false "tree_member 27" (tree_member 37 t14);

    assert_true  "tree_member 28" (tree_member 16 t15);
    assert_false "tree_member 29" (tree_member 27 t15);

    assert_true  "tree_member 30" (tree_member 38 t16);
    assert_false "tree_member 31" (tree_member 91 t16);

    assert_true  "tree_member 32" (tree_member 65 t17);
    assert_false "tree_member 33" (tree_member 27 t17);

    assert_true  "tree_member X" (tree_member 91 t18);
    assert_false "tree_member X" (tree_member 0  t18);

    assert_true  "tree_member X" (tree_member 27 t19);
    assert_false "tree_member X" (tree_member (-1) t19);
  );

  "Part B: insert" >:: (fun _ ->
    assert_equal ~msg:"insert 1" t1 t1_correct;
    assert_equal ~msg:"insert 2" t2 t2_correct;
    assert_equal ~msg:"insert 3" t3 t3_correct;
    assert_equal ~msg:"insert 4" t4 t4_correct;
    assert_equal ~msg:"insert 5" t5 t5_correct;
    assert_equal ~msg:"insert 6" t6 t6_correct;
    assert_equal ~msg:"insert 7" t7 t7_correct;
    assert_equal ~msg:"insert 8" t8 t8_correct;
    assert_equal ~msg:"insert 9" t9 t9_correct;
    assert_equal ~msg:"insert 10" t10 t10_correct;
    assert_equal ~msg:"insert 11" t11 t11_correct;
    assert_equal ~msg:"insert 12" t12 t12_correct;
    assert_equal ~msg:"insert 13" t13 t13_correct;
    assert_equal ~msg:"insert 14" t14 t14_correct;
    assert_equal ~msg:"insert 15" t15 t15_correct;
    assert_equal ~msg:"insert 16" t16 t16_correct;
    assert_equal ~msg:"insert 17" t17 t17_correct;
    assert_equal ~msg:"insert 18" t18 t18_correct;
    assert_equal ~msg:"insert 19" t19 t19_correct;
  );

  (* Check if inserting random values into a tree preserves the
   * orderedness property of the tree. *)
  "Part B: random inserts: ordered" >:: (fun _ ->
    let n_random_tests = 1000 in
      for i = 1 to n_random_tests do
        let lst = random_list 20 100 in
        (* let _ = print_int_list lst in *)
        let tree = tree_of_list lst in
          if not (ordered tree) then
            let msg = Printf.sprintf "insert_ordered %d" i in
              begin
                Printf.printf "%s: unordered tree after insert.\n" msg;
                Printf.printf "  Tree derived from this list:\n";
                print_int_list lst;
                assert_false msg false;
              end
      done;
  );

  "Part B: random inserts: balanced" >:: (fun _ ->
    let n_random_tests = 1000 in
      for i = 1 to n_random_tests do
        let lst = random_list 20 100 in
        (* let _ = print_int_list lst in *)
        let tree = tree_of_list lst in
          if not (balanced tree) then
            let msg = Printf.sprintf "insert_ordered %d" i in
              begin
                Printf.printf "%s: unbalanced tree after insert.\n" msg;
                Printf.printf "  Tree derived from this list:\n";
                print_int_list lst;
                assert_false msg false;
              end
      done;
  );

]

let _ = run_test_tt_main all_tests

