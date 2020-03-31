(*  search.ml: search strategies  *)
(*  Student name: Marcus Gee      *)
(*  CMS cluster login name: mcgee *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end

module Search (S : Storage) (D : Domain) =
  struct
    module DS = Set.Make(D)

    let rec iter q visited = 
        if S.is_empty q 
        then raise Not_found
        else 
            let rec_hist = S.pop q in 
            if DS.mem (List.hd rec_hist) visited 
            then iter q visited
            else 
                 if D.is_solved (List.hd rec_hist)
                 then rec_hist
                 else 
                    let updated_visited = DS.add (List.hd rec_hist) visited in
                    let child = D.next (List.hd rec_hist) in 
                    begin 
                        List.iter (fun x -> S.push (x :: rec_hist) q) child;
                        iter q updated_visited 
                    end
 
    let search init = 
        let q = S.create () in
        let history = [init] in 
        let visited = DS.empty in 
        begin 
            S.push history q;
            iter q visited;
        end 
    
    let show_history hist =
      (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"
  end

