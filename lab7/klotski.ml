(* klotski.ml: core functionality of the Klotski game. *)
(* Student name:                *)
(* CMS cluster login name:      *)

(* ---------------------------------------------------------------------- 
 * Types.
 * ---------------------------------------------------------------------- *)

type loc = int * int
type dir = Up | Down | Left | Right
type move = char * dir * int

module LocM =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module LocSet : Set.S with type elt = loc = Set.Make(LocM)

(* Sets of LocSets.  Used locally only. *)

module LocSetM =
  struct
    type t = LocSet.t
    let compare = LocSet.compare
  end

module LocSetSet = Set.Make(LocSetM)

module CharM =
  struct
    type t = char
    let compare = Stdlib.compare
  end

module CharMap : Map.S with type key = char = Map.Make(CharM)

type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

(* ---------------------------------------------------------------------- 
 * Functions.
 * ---------------------------------------------------------------------- *)

(* Create a board from a string. *)
let read s = 
  let rec iter p u r c =
    match () with
      | _ when r = 5 -> { pieces = p; unoccupied = u }
      | _ when c = 4 -> iter p u (r + 1) 0 
      | _ -> 
        let i = r * 4 + c in
        let ch = s.[i] in
          if ch = '.'  (* unoccupied location; add to unoccupied set *)
            then iter p (LocSet.add (r, c) u) r (c + 1)
            else  (* occupied; add to appropriate piece set *)
              try
                let cs  = CharMap.find ch p in     (* old piece set *)
                let cs' = LocSet.add (r, c) cs in  (* add new location *)
                let p'  = CharMap.add ch cs' p in  (* store back into map *)
                  iter p' u r (c + 1)
              with
                Not_found ->  (* new piece; create a new piece set *)
                  let cs = LocSet.singleton (r, c) in
                  let p' = CharMap.add ch cs p in
                    iter p' u r (c + 1)
  in
    if String.length s <> 20
      then failwith "read: invalid input string length"
      else iter CharMap.empty LocSet.empty 0 0

(* Convert the board to a string representation suitable for printing. *)
let show b = 
  let string_of_char_list = function
    | [a;b;c;d] -> Printf.sprintf "%c%c%c%c" a b c d
    | _ -> failwith "invalid char list"
  in
  let char_at board loc =
    let rec iter = function
      | [] -> raise Not_found
      | (c, locs) :: t -> 
        if LocSet.mem loc locs then c else iter t
    in
    if LocSet.mem loc board.unoccupied
      then '.'
      else iter (CharMap.bindings board.pieces)
  in
  (String.concat "\n"
     (List.map (fun r ->
        string_of_char_list
          (List.map (char_at b) 
            (List.map (fun c -> (r, c)) [0; 1; 2; 3])))
        [0; 1; 2; 3; 4])) ^ "\n"

let is_solved b = 
    let solved_loc = LocSet.of_list [(3,1); (3,2); (4,1); (4,2)] in
    CharMap.exists (fun pieces winning_loc -> 
                            LocSet.equal solved_loc winning_loc) b.pieces 
    
let compare b1 b2 = 
    let map1 = CharMap.bindings b1.pieces in 
    let map2 = CharMap.bindings b2.pieces in
    let set1 = LocSetSet.of_list (snd (List.split map1)) in 
    let set2 = LocSetSet.of_list (snd (List.split map2)) in
    LocSetSet.compare set1 set2
    
let remove c ({ pieces = p; unoccupied = u } as b) = 
    if CharMap.mem c p
    then {pieces = CharMap.remove c p; 
          unoccupied = LocSet.union (CharMap.find c p) u}
    else b

let add (c, p) { pieces = ps; unoccupied = u } = 
  if (LocSet.subset p u) && (not (CharMap.mem c ps))
  then Some {pieces = CharMap.add c p ps; unoccupied = LocSet.diff u p}
  else None

let helper_check c p d i b = 
    let rec iter c p d i b n = 
        let updated_p = 
            LocSet.map (fun (x, y) -> 
                        match d with    
                        | Up    -> (x - n, y)
                        | Down  -> (x + n, y)
                        | Left  -> (x, y - n)
                        | Right -> (x, y + n)) p in
        if n = i
        then match (add (c, updated_p) b) with
             | Some s -> true
             | None   -> false
        else match (add (c, updated_p) b) with
             | Some s -> iter c p d i b (n + 1)   
             | None   -> false     
     in 
     iter c p d i b 1

let make_move (c, d, i) b =
    if (not (CharMap.mem c b.pieces)) || (i < 1) || (i  > 4) 
    then None
    else 
        let curr = CharMap.find c b.pieces in
        let board = remove c b in 
        if helper_check c curr d i board
        then let updated_p = 
            LocSet.map (fun (x, y) -> 
                        match d with    
                        | Up    -> (x - i, y)
                        | Down  -> (x + i, y)
                        | Left  -> (x, y - i)
                        | Right -> (x, y + i)) curr in
        add (c, updated_p) board
        else None

let get_moves d c i b = 
    let rec iter d c i b l = 
        match make_move (c, d, i) b with 
        | Some s -> iter d c (i + 1) b (s :: l)
        | None -> l
    in
    iter d c i b []
        


let get_directions c i b =
    (get_moves Up c i b) @ 
    (get_moves Down c i b) @ 
    (get_moves Left c i b) @ 
    (get_moves Right c i b)                         
  
let next b =
    let map = CharMap.bindings b.pieces in 
    let chars = fst (List.split map) in 
    let rec iter c b boards = 
        match c with 
        | []       -> boards
        | (h :: t) -> iter t b ((get_directions h 1 b) @ boards)
    in iter chars b []
   
(* Function to interactively test the "next" function. 
 * Useful for debugging. *)
let test_next b =
  let bs = next b in
    begin
      print_string (show b ^ "\n");
      List.iter 
        (fun b -> print_string ("----\n\n" ^ show b ^ "\n"))
        bs
    end

