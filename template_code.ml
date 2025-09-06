(* Note: we have multiple definitions for type tree and reuse the function names 
   so to run any of the algorithms you need to comment out the other ones *)
exception NotImplemented

(* Naive Algorithm *) 
type tree = Empty | Node of int * tree * tree
                            
(* Returns optimum fun for t *)
let rec party (t: tree) : int = 
  raise NotImplemented

(* Returns optimum fun for t assuming the root node of t is included *)
and party_in (t: tree) : int = 
  raise NotImplemented

(* Returns optimum fun for t assuming the root node of t is excluded *)
and party_out (t: tree) : int =
  raise NotImplemented

(* Memoized Algorithm *) 
type tree = Empty
          | Node of int * string * tree * tree *
                    ((int*string list) option) ref

let rec party (t: tree) : int * string list =
  raise NotImplemented

and party_in (t: tree) : int * string list =
  raise NotImplemented

and party_out (t: tree) : int * string list =
  raise NotImplemented
  
(* Tail-recursive Algorithm *) 
type tree = Empty
          | Node of int * string * tree * tree *
                    ((int*string list) option) ref

let rec party_tr (t: tree) (cont: int * string list -> 'a) : 'a = 
  raise NotImplemented

and party_in_tr (t: tree) (cont: int * string list -> 'a) : 'a = 
  raise NotImplemented
             
and party_out_tr (t: tree) (cont: int * string list -> 'a) : 'a =
  raise NotImplemented
  
(* Generalized Solution: Tail-recursive for list of children *)
type tree = 
  | Empty
  | Node of int * string * tree list * ((int * string list) option) ref

let rec party_tr (t: tree) (cont: int * string list -> 'a) : 'a = 
  raise NotImplemented
  
and party_in_tr (t: tree) (cont: int * string list -> 'a) : 'a = 
  raise NotImplemented 

and party_out_tr (t: tree) (cont: int * string list -> 'a) : 'a = 
  raise NotImplemented 