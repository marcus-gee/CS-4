(* Student name: Marcus Gee *)
(* CMS cluster login name: mcgee *)

open Boardrep

(* ---------------------------------------------------------------------- *)
(* Helper functions *)
let square n = n * n 

let int_to_loc n size = (n / size, n mod size)

let abs n = if n < 0 then 0 - n else n
(* ---------------------------------------------------------------------- *)


module type BoardMetric =
  sig
    type t

    val distance : t -> t -> int
  end

module Hamming(B : BoardRep) : BoardMetric with type t = B.t =
  struct
    type t = B.t

    let distance b1 b2 =
      let rec hamming_count b1 b2 n count =
          match n with 
          | x when x = (square (B.get_size b1)) -> count
          | _ -> if (B.get b2 (int_to_loc n (B.get_size b2)) <> 0) &&
                    (B.get b1 (int_to_loc n (B.get_size b1)) <> 
                        B.get b2 (int_to_loc n (B.get_size b2))) 
                 then hamming_count b1 b2 (n + 1) (count + 1)
                 else hamming_count b1 b2 (n + 1) count
      in
        if (B.get_size b1) <> (B.get_size b2)
        then failwith "incompatible board sizes"
        else hamming_count b1 b2 0 0
  end

module Manhattan(B : BoardRep) : BoardMetric with type t = B.t =
  struct
    type t = B.t  

    let rec loc_from_val b val_ n =
      let size = B.get_size b in
      match B.get b (n / size, n mod size) with 
      | x when x = val_ -> (n / size, n mod size)
      | _ -> loc_from_val b val_ (n + 1)

    let loc_diff (r1, c1) (r2, c2) = abs (r2 - r1) + abs(c2 - c1)
      
    let distance b1 b2 =
      let rec manhattan_sum b1 b2 n sum = 
        if n = (square (B.get_size b1)) 
        then sum
        else manhattan_sum b1 b2 (n + 1) 
                   (sum + loc_diff (loc_from_val b1 n 0) (loc_from_val b2 n 0))
      in
        if (B.get_size b1) <> (B.get_size b2)
        then failwith "incompatible board sizes"
        else manhattan_sum b1 b2 1 0
  end

