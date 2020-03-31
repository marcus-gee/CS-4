(* Student name: Marcus Gee *)
(* CMS cluster login name: mcgee *)

type loc = int * int
type move = Up | Down | Left | Right

module type BoardRep =
  sig
    type t

    exception Invalid_move
    exception Invalid_location

    val init      : int -> t
    val load      : int -> int list -> t
    val get_size  : t -> int
    val get_hole  : t -> loc
    val get       : t -> loc -> int
    val make_move : t -> move -> t
    val show      : t -> unit
  end

(* ---------------------------------------------------------------------- 
 * Helper functions.
 * ---------------------------------------------------------------------- *)

(* Make a display function given board accessors. *)
let make_show get get_size b =
  let size = get_size b in
    begin
      Printf.printf "\n%!";
      for row = 0 to size - 1 do
        for col = 0 to size - 1 do
          Printf.printf "%3d" (get b (row, col))
        done;
        Printf.printf "\n";
      done;
      Printf.printf "\n%!"
    end
 
let square n = n * n 
   
(* checks if list is proper length *)
let valid_length l size = Array.length (Array.of_list l) = (size * size)

(* checks if list has correct elements *)
let invalid_contents l size = 
    let rec repeats l = 
      match l with 
      | [] -> false 
      | h :: t -> if List.mem h t
                  then true 
                  else repeats t
    in
      Array.exists (fun x -> x < 0) (Array.of_list l) || 
      Array.exists (fun x -> x > ((square size) - 1)) (Array.of_list l) ||
      repeats l
      
(* finds the loc of a given board *)
let rec find_hole lst size idx = 
      match lst with 
      | [] -> failwith "no hole"
      | h :: t -> if h = 0 
                  then (idx / size, idx mod size) 
                  else find_hole t size (idx + 1)

(* checks if move puts hole off of board *)  
let out_of_bounds loc size m = 
  match m with
  | Up    -> fst (loc) = 0
  | Down  -> fst (loc) = size - 1
  | Right -> snd (loc) = size - 1
  | Left  -> snd (loc) = 0

(* if move is valid update loc of hole *)
let update_hole m (r, c) = 
    match m with
    | Up    -> (r - 1, c)
    | Down  -> (r + 1, c)
    | Right -> (r, c + 1)
    | Left  -> (r, c - 1)

(* ---------------------------------------------------------------------- 
 * Modules.
 * ---------------------------------------------------------------------- *)

(* ----------------------------------------------------------------------
 * ArrayRep 
 * ---------------------------------------------------------------------- *)
module OrderedLoc =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module ArrayRep : BoardRep =
  struct
    type t = 
      { 
        contents : int array;
        size : int;
        hole : loc
      }

    exception Invalid_move
    exception Invalid_location

    let init size = 
      let rec make_contents n lst =
        if n = 1 
        then lst @ [0]
        else make_contents (n - 1) ((n - 1) :: lst) 
      in  
        if size < 2 
        then failwith "ERROR: init: size must be at least 2"
        else
          { contents = Array.of_list (make_contents (square size) []); 
            size = size; 
            hole = (size - 1, size - 1) 
          }    
                                    
    let load size lst =
      if size < 2 then
        failwith "ERROR: load: size must be at least 2"
      else 
        if not (valid_length lst size)
        then failwith "invalid list length"
        else if (invalid_contents lst size)
        then failwith "invalid list contents"
        else
          { contents = Array.of_list lst; 
            size = size; 
            hole = find_hole lst size 0
          }

    let get_size b = b.size

    let get_hole b = b.hole

    let get { contents; size = s } (r, c) = 
      if r < 0 || r > s - 1 || c < 0 || c > s - 1 then
        raise Invalid_location
      else
        try
          contents.(r * s + c)
        with (Invalid_argument _) ->
          raise Invalid_location
    
    let update_board_array b m = 
      let old_idx = (fst b.hole) * b.size + (snd b.hole) in
      let (r, c)  = update_hole m b.hole in
      let new_idx = (r * b.size) + c in
      let moved_tile = Array.get b.contents new_idx
      in
        let updated = Array.copy b.contents in
          updated.(old_idx) <- moved_tile;
          updated.(new_idx) <- 0;
        { b with contents = updated; 
                 hole = (new_idx / b.size, new_idx mod b.size) }

    let make_move b m =
      if (out_of_bounds b.hole b.size m)
      then raise Invalid_move
      else update_board_array b m
        

    let show = make_show get get_size
  end
  
(* ----------------------------------------------------------------------
 * MapRep 
 * ---------------------------------------------------------------------- *)
module MapRep : BoardRep =
  struct
    module LocMap = Map.Make(OrderedLoc)

    type t = 
      { 
        contents : int LocMap.t;
        size : int;
        hole : loc
      }

    exception Invalid_move
    exception Invalid_location

    let init size =
      let rec make_contents size n map =
          match n with 
          | x when x = ((square size) - 1) -> 
                            LocMap.add (n / size, n mod size) 0 map
          | _ -> make_contents size (n + 1)  
                            (LocMap.add (n / size, n mod size) (n+1) map)
        in
      if size < 2 
      then failwith "ERROR: init: size must be at least 2"
      else
        { contents = make_contents size 0 LocMap.empty;
          size = size;
          hole = (size - 1, size - 1) 
        }

    let load size lst =
      if size < 2 
      then failwith "ERROR: load: size must be at least 2"
      else 
        if not (valid_length lst size)
        then failwith "invalid list length"
        else if (invalid_contents lst size )
        then failwith "invalid list contents"
        else
          let rec make_contents lst idx map =
            match lst with 
            | [] -> map
            | h :: t -> make_contents t (idx + 1) 
                            (LocMap.add (idx / size, idx mod size) h map)
          in                   
          { contents = make_contents lst 0 LocMap.empty; 
            size = size; 
            hole = find_hole lst size 0 
          }

    let get_size b = b.size

    let get_hole b = b.hole

    let get { contents } l = 
      try
        LocMap.find l contents
      with Not_found ->
        raise Invalid_location

    let update_board_map b m = 
      let old_loc = b.hole in
      let new_loc  = update_hole m b.hole in
      let moved_tile = LocMap.find new_loc b.contents
      in
        let updated =
                LocMap.add new_loc 0 (LocMap.add old_loc moved_tile b.contents)
        in 
          { b with contents = updated;  hole = new_loc }

    let make_move b m =
      if (out_of_bounds b.hole b.size m)
      then raise Invalid_move
      else update_board_map b m
        

    let show = make_show get get_size
  end

