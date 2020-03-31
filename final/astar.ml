(* Student name: Marcus Gee *)
(* CMS cluster login name: mcgee *)

open Pqueue

module type Task =
  sig
    type t

    val compare : t -> t -> int
    val eval    : t -> t -> int
    val next    : t -> t list
    val display : t -> string
  end

module AStar (T : Task) =
  struct
    (* Raised when no more tasks are left on the queue. *)
    exception No_solution

    (* Raised when a solution is encountered while searching. *)
    exception Solved of T.t list

    (* The state of a search stored in the priority queue *)
    type qstate = {
      task : T.t;

      (* the list of task states that preceded this one in the search *)
      history : T.t list;  

      (* the number of moves to get to this state; this is just the length
         of the history list *)
      nmoves : int;   

      (* the overall fitness: a function of the task evaluation and nmoves *)
      fitness : int    
    }

    (* Make the priority queue.  Compare queue states by fitness. *)
    module Tqueue = MakePriorityQueue(struct
        type t = qstate
        let compare s1 s2 = Stdlib.compare s1.fitness s2.fitness
      end)

    (* A map of the best quality scores for each evaluated task. *)
    module Tmap = Map.Make(T)

    (* The state of the solver as a whole. *)
    type t = {
      queue : Tqueue.t;
      best  : int Tmap.t
    }

    (* The solver function. *)
    let solve goal init =
      let init_eval  = T.eval goal init in
      let init_state =
        {
           task    = init;
           history = [];
           nmoves  = 0;
           fitness = init_eval;
        }
      in
      let init_queue  = Tqueue.insert (Tqueue.empty) init_state in
      let init_best   = Tmap.empty in
      let init_solver = { queue = init_queue; best = init_best } in

      (* 
         The main solving loop using the priority queue.
       
         Invariant: tasks on the queue are not solved.

         1) Pop a state value off the queue.  If the queue is empty,
            there is no solution.  Raise a No_solution exception.
      
         2) Check if the task in the popped queue state is a key in the `best`
            map: If it isn't, add it to the `best` map as a key with the number
            of moves leading to the task state (the `nmoves` field of the queue
            state) as the value.  If it is, compare it to the move count in the
            map.  If the new number of moves is smaller than the one in the map,
            replace the binding in the map.  Otherwise, this task has been
            already searched for with a smaller (or at least no larger) number
            of moves, so there is no point in searching it again, so discard it
            and restart the loop.

         3) Assuming we got this far, compute the next queue states.
            Check to see if any of them contains a task which is a solution; if
            so, compute the final list of tasks and raise a Solved exception to
            break out of the loop.  Otherwise, add them back into the queue and 
            continue.  
       *)

      let rec iter { queue; best }  =
        let (state, new_q) = Tqueue.pop_min queue in
        if (not (Tmap.mem (state.task) best)) || 
           (state.nmoves < Tmap.find state.task best)
        then 
          begin 
            let new_best    = Tmap.add state.task state.nmoves best in
            let new_history = (state.task :: state.history) in 
            let new_nmoves  = state.nmoves + 1 in
            let next_moves  = T.next state.task in  
            let new_q = List.fold_left (fun qq t ->
                          if T.eval t goal <> 0
                          then Tqueue.insert qq ({ task    = t; 
                                                   history = new_history;
                                                   nmoves  = new_nmoves;
                                                   fitness = 
                                                   (T.eval t goal) + new_nmoves; 
                                                 })
                          else raise (Solved (List.rev (t :: new_history)))
                              ) queue next_moves;
            in 
              iter { queue = new_q ; best  = new_best }
          end
        else iter { queue = new_q ; best }        
      in
        (* The main part of the function starts here. *)
        if init_eval = 0 then
          [init]  (* handle the case when the initial state is solved. *)
        else
          try
            iter init_solver
          with 
            | Tqueue.Empty -> raise No_solution
            | Solved tlist -> tlist
  end
