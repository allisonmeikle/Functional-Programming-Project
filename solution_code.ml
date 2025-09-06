(* The first two provided algorithms comes from section 12.5 of the textbook: 
   Functional programming in OCaml
https://courses.cs.cornell.edu/cs3110/2021sp/textbook/adv/memoization.html *)
  
(* Note: we have multiple definitions for type tree and reuse the function names 
   so to run any of the algorithms you need to comment out the other ones *)

(* Naive Algorithm *) 
type tree = Empty | Node of int * tree * tree
                            
(* Returns optimum fun for t. *)
let rec party(t) = max (party_in t) (party_out t)

(* Returns optimum fun for t assuming the root node of t is included. *)
and party_in(t) =
  match t with 
    Empty -> 0
  | Node(v,left,right) -> v + party_out(left)
                          + party_out(right)

(* Returns optimum fun for t assuming the root node of t is excluded. *)
and party_out(t) =
  match t with
    Empty -> 0
  | Node(v,left,right) -> party(left) + party(right)

(* Memoized Algorithm *)
(* This version memoizes the optimal fun value for each tree node.
     It also remembers the best invite list. Each tree node has the
     name of the employee as a string. *) 

(* Using this for the sake of our proof instead of @ *)
let rec append l1 l2 = 
  match l1 with 
  | [] -> l2
  | h::t -> h :: (append t l2) 

type tree = Empty
          | Node of int * string * tree * tree *
                    ((int*string list) option) ref

let rec party(t): int * string list =
  match t with
    Empty -> (0, [])
  | Node(v,name,left,right,memo) ->
      (match !memo with
         Some result -> result
       | None ->
           let (infun, innames) = party_in(t) in
           let (outfun, outnames) = party_out(t) in
           let result =
             (* this is the line that was changed from the textbook *)
             if infun > outfun then (infun, innames)
             else (outfun, outnames)
           in
           (memo := Some result); result)

and party_in(t) =
  match t with
    Empty -> (0, [])
  | Node(v,name,l,r,_) ->
      let (lfun, lnames) = party_out(l)
      and (rfun, rnames) = party_out(r) in
      (* also changed to use function append for the sake of our proof *)
      (v + lfun + rfun, name :: (append lnames rnames))

and party_out(t) =
  match t with
    Empty -> (0, [])
  | Node(v,_,l,r,_) ->
      let (lfun, lnames) = party(l)
      and (rfun, rnames) = party(r) in
      (* also changed to use function append for the sake of our proof *)
      (lfun + rfun, append lnames rnames)
  
(* Our Solution: Tail-recursive Algorithm *) 
let append_tr l1 l2 = 
  let rec append_helper l1 l2 cont = 
    match l1 with 
    | [] -> cont l2
    | h::t -> append_helper t l2 (fun r -> cont (h::r))
  in
  append_helper l1 l2 (fun r -> r) 
    
type tree = Empty
          | Node of int * string * tree * tree *
                    ((int*string list) option) ref

let rec party_tr (t: tree) (cont: int * string list -> 'a) : 'a = 
  match t with 
  | Empty -> cont (0, [])
  | Node (v, name, _, _, memo) -> 
      match !memo with 
      | Some result -> cont result
      | None ->
          party_in_tr t (fun (infun, innames) -> 
              party_out_tr t (fun (outfun, outnames) -> 
                  let result = 
                    if infun > outfun then (infun, innames)
                    else (outfun, outnames)
                  in
                  memo := Some result; cont result)
            )

and party_in_tr (t: tree) (cont: int * string list -> 'a) : 'a = 
  match t with 
  | Empty -> cont (0, [])
  | Node (v, name, l, r, memo) -> 
      party_out_tr l (fun (lfun, lnames) -> 
          party_out_tr r (fun (rfun, rnames) -> 
              let result = (v + lfun + rfun, name::(append_tr lnames rnames))
              in 
              cont result
            )
        )
             
and party_out_tr (t: tree) (cont: int * string list -> 'a) : 'a =
  match t with 
  | Empty -> cont (0, [])
  | Node(v, name, l, r, memo) ->
      party_tr l (fun (lfun, lnames) -> 
          party_tr r (fun (rfun, rnames) -> 
              let result = (lfun + rfun, append_tr lnames rnames) in
              cont result
            )
        )
  
(* Driver function *)
let party_tr_call (t: tree) = 
  party_tr t (fun r -> r)
  
(* Generalized Solution: Tail-recursive for list of children *)
type tree = 
  | Empty
  | Node of int * string * tree list * ((int * string list) option) ref

let rec party_tr (t: tree) (cont: int * string list -> 'a) : 'a = 
  match t with 
  | Empty -> cont (0, [])
  | Node (v, name, children, memo) -> 
      match !memo with 
      | Some result -> cont result
      | None ->
          party_in_tr t (fun (infun, innames) -> 
              party_out_tr t (fun (outfun, outnames) -> 
                  let result = 
                    if infun > outfun then (infun, innames)
                    else (outfun, outnames)
                  in
                  memo := Some result; cont result))

and party_in_tr (t: tree) (cont: int * string list -> 'a) : 'a = 
  match t with 
  | Empty -> cont (0, [])
  | Node (v, name, children, memo) -> 
      process_children_out children (fun (child_fun, child_names) -> 
          let result = (v + child_fun, name :: child_names) in 
          cont result
        )

and party_out_tr (t: tree) (cont: int * string list -> 'a) : 'a =
  match t with 
  | Empty -> cont (0, [])
  | Node (_, _, children, _) ->
      process_children_in children (fun (child_fun, child_names) -> 
          let result = (child_fun, child_names) in
          cont result
        )

(* Helper function to process a list of trees for "in" computations *)
and process_children_in (children: tree list) (cont: int * string list -> 'a) : 'a =
  let rec aux acc_fun acc_names remaining cont =
    match remaining with
    | [] -> cont (acc_fun, acc_names)
    | child :: rest ->
        party_tr child (fun (child_fun, child_names) ->
            aux (acc_fun + child_fun) (append_tr acc_names child_names) rest cont)
  in
  aux 0 [] children cont

(* Helper function to process a list of trees for "out" computations *)
and process_children_out (children: tree list) (cont: int * string list -> 'a) : 'a =
  let rec aux acc_fun acc_names remaining cont =
    match remaining with
    | [] -> cont (acc_fun, acc_names)
    | child :: rest ->
        party_out_tr child (fun (child_fun, child_names) ->
            aux (acc_fun + child_fun) (append_tr acc_names child_names) rest cont)
  in
  aux 0 [] children cont 

(* Driver function *)
let party_tr_call (t: tree) = 
  party_tr t (fun r -> r) 