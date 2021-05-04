(* Implementacja leftist.mli *)
(* Author: Tomasz Ziebowicz  *)

(* ----- Definicje typów ----- *)

type 'a queue = 
  | None
  | Some of {
    (* Priorytet *)
    pri : 'a;
    
    (* Prawa wysokość drzewa reprezentującego kolejkę *)
    r_hgt : int;
    
    (* Lewe poddrzewo*)
    l : 'a queue;
    
    (* Prawe poddrzewo*)
    r : 'a queue; 
  }

(* ----- Definicje wyjątków ----- *)

exception Empty

(* ----- Definicje stałych ----- *)

let empty = None

(* ----- Konstruktory ----- *)

(* 
 * Funckja zwraca jednoelementową kolejkę typu "'a queue" 
 * zawierającą priorytet "p" typu "'a". 
 *)
let create p = 
  Some {
    pri = p;
    r_hgt = 0;
    l = None;
    r = None;
  }

(* ----- Selektory ----- *)

let is_empty = function
| None -> true
| Some _ -> false

(* ----- Modyfikatory ----- *)

let rec join q1 q2 = 
  match q1, q2 with
  | None, None -> None
  | Some _, None -> q1
  | None, Some _ -> q2
  | Some qa, Some qb when qa.pri > qb.pri -> join q2 q1
  | Some qa, Some qb ->
      let child1, child2 = qa.l, join qa.r q2 in
      match child1, child2 with
      | None, Some _ -> Some {pri = qa.pri; r_hgt = 0; l = child2; r = None}
      | Some a, Some b when a.r_hgt < b.r_hgt ->
          Some {pri = qa.pri; r_hgt = a.r_hgt + 1; l = child2; r = child1}
      | Some _, Some b ->
          Some {pri = qa.pri; r_hgt = b.r_hgt + 1; l = child1; r = child2}

let delete_min q =
  match q with
  | None -> raise Empty
  | Some a -> a.pri, join a.l a.r          

let add p q = join (create p) q
