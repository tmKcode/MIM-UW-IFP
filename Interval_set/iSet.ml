(*
 * ISet - Interval sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl,
 * Jacek Chrzaszcz, Tomasz Ziębowicz
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
 
(* ----- Definicje typów ----- *)

(* Kolejno: lewe poddrzewo, przedział postaci (int * int), prawe poddrzewo,
   wysokość drzewa, liczba różnych liczb całkowitych 
   zawartych w elementach drzewa *)
type t =
  | Empty
  | Node of t * (int * int) * t * int * int

(* ----- Definicje stałych ----- *)

let empty = Empty

(* ----- Funkcje pomocnicze ----- *)

let safe_succ x =
  let s = succ x in
  if s < x then max_int
  else s

let safe_pred x =
  let p = pred x in
  if p > x then min_int
  else p

let is_valid (a, b) = 
  a <= b

(*
 * Założenie: Rzeczywiście x + y >= 0. 
 *)
let safe_add x y =
  let s = x + y in
  if s < 0 then max_int
  else s

(* Jeśli obcinamy z przedziału wszystkie elementy większe lub równe min_int,
 * funkcja zwraca niewłaściwy przedział 
 *)  
let cut_right (a, b) x =
  if x = min_int then (1, 0) else (a, safe_pred x)

(*
 * Jeśli obcinamy z przedziału wszystkie elementy mniejsze lub równe max_int,
 * funkcja zwraca niewłaściwy przedział
 *)  
let cut_left (a, b) x =
  if x = max_int then (1, 0) else (safe_succ x, b)

(* 
 * Funkcja zwraca liczbę liczb całkowitych, 
 * które znajdują się w przedziale (x, y).
 * Maksymalnie max_int. 
 *)
let i_card (x, y) =
  let d = y - x + 1 in
  if d <= 0 then max_int
  else d

(* ----- Selektory ----- *)

let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

let card t =
  match t with
  | Node (_, _, _, _, c) -> c
  | Empty -> 0

let is_empty x = 
  x = Empty

let rec min_elt = function
  | Node (Empty, i, _, _, _) -> i
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(* ----- Konstruktory ----- *)

let make l i r = Node (l, i,  r, max (height l) (height r) + 1,
  safe_add (safe_add (card l) (card r)) (i_card i)) 
  
(* ----- Modyfikatory ----- *)

(* 
 * Wszystkie modyfikatory zakładają, 
 * że dane im drzewo/drzewa spełniają założenia BST,
 * oraz że przechowywane w nich przedziały są rozłączne 
 * wg kryterium z polecenia. 
 *)

(* 
 * Wszystkie modyfikatory zwracają zbalansowane 
 * wg. kryterium AVL drzewa spełniające
 * załozenia BST. Należące do nich przedziały są 
 * rozłączne wg kryterium z polecenia. 
 *)

let bal l i r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, li, lr, _, _) ->
        if height ll >= height lr then make ll li (make lr i r)
        else
          (match lr with
          | Node (lrl, lri, lrr, _, _) ->
              make (make ll li lrl) lri (make lrr i r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, ri, rr, _, _) ->
        if height rr >= height rl then make (make l i rl) ri rr
        else
          (match rl with
          | Node (rll, rli, rlr, _, _) ->
              make (make l i rll) rli (make rlr ri rr)
          | Empty -> assert false)
    | Empty -> assert false
  else 
    make l i r

let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, i,  r, _, _) -> bal (remove_min_elt l) i r
  | Empty -> invalid_arg "ISet.remove_min_elt"

(* 
 * Funkcja zwraca:
 * 0, x jest elementem przedziału (a + 1, b + 1)
 * 1, x jest po prawej od tego przedziału
 * -1, w p.p. 
 *)
let i_cmp x (a, b) = 
  if a <= safe_succ x && safe_pred x <= b then 0
  else
    if safe_succ x < a then -1
    else 1

(* 
 * Funkcja zwraca:
 * 0, x jest elementem przedziału (a, b)
 * 1, x jest po prawej od tego przedziału
 * -1, w p.p. 
 *)
let cmp x (a, b) = 
  if a <= x && x <= b then 0
  else
    if x < a then -1
    else 1


(* 
 * Funkcja zwraca dane drzewo, z dodanym przedziałem x.
 * Założenie: Przedział x jest rozłączny (wg. kryterium zadania)
 * z każdym przedziałem w danym drzewie. 
 *)
let rec add_disjoint x = function
  | Node (l, i, r, h, c) ->
      let rel = i_cmp (fst x) i in
      if rel = 0 then Node (l, x, r, h, c)
      else if rel < 0 then
        let nl =  add_disjoint x l in
        bal nl i r
      else
        let nr =  add_disjoint x r in
        bal l i nr
  | Empty -> Node (Empty, x, Empty, 1, i_card x)

let rec join l i r =
  match (l, r) with
  | (Empty, _) ->  add_disjoint i r
  | (_, Empty) ->  add_disjoint i l
  | (Node(ll, li, lr, lh, _), Node(rl, ri, rr, rh, _)) ->
      if lh > rh + 2 then bal ll li (join lr i r) else
      if rh > lh + 2 then bal (join l i rl) ri rr else
      make l i r
  
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let i = min_elt t2 in
      join t1 i (remove_min_elt t2)

let rec split x = function
    Empty ->
      (Empty, false, Empty)
  | Node (l, i, r, _, _) ->
      let rel = cmp x i in
      if rel = 0 then
        let lrest = cut_right i x
        and rrest = cut_left i x in
        match is_valid lrest, is_valid rrest with
        | true, true -> (add_disjoint lrest l, true, add_disjoint rrest r)
        | true, false -> (add_disjoint lrest l, true, r)
        | false, true -> (l, true, add_disjoint rrest r)
        | false, false -> (l, true, r)
      else if rel < 0 then
        let (ll, pres, rl) = split x l in (ll, pres, join rl i r)
      else
        let (lr, pres, rr) = split x r in (join l i lr, pres, rr)

(* 
 * Funkcja zwraca parę (l, a), gdzie:
 * 
 * -a jest:
 *   -lewym krańcem przedziału (a, b), gdy x nalezy do przedziału (a, b + 1)
 *   -x w.p.p
 * 
 * -l jest drzewem wszystkich przedziałów z danego drzewa,
 *  które leżą na lewo od a. 
 *)
let rec split_left x = function
  | Empty ->
      (Empty, x)
  | Node (l, (a, b), r, _, _) ->
      if a <= x && safe_pred x <= b then (l, a)
      else if x < a then
        split_left x l
      else
        let (lr, e) = split_left x r in (join l (a, b) lr, e)

(* Funkcja zwraca parę (b, r), gdzie:
 * 
 * -b jest:
 *   -prawym krańcem przedziału (a, b), gdy x nalezy do przedziału (a + 1, b)
 *   -x w.p.p.
 *  
 * -r jest drzewem wszystkich przedziałów z danego drzewa,
 *  które leżą na prawo od b. 
 *)
let rec split_right x = function
  | Empty ->
      (x, Empty)
  | Node (l, (a, b), r, _, _) ->
      if a <= safe_succ x && x <= b then (b, r)
      else if b < x then
        split_right x r
      else
        let (e, rl) = split_right x l in (e, join rl (a, b) r)

let add (a, b) = function
  | Empty ->
      Node (Empty, (a, b), Empty, 1, i_card (a, b))  
  | Node _ as t ->
      let lrest, l = split_left a t
      and r, rrest = split_right b t
      in
      join lrest (l, r) rrest

let remove (a, b) = function
  | Empty ->
      Empty
  | Node _ as n ->
      let (l, _, _) = split a n
      and (_, _, r) = split b n in
      merge l r
        
let mem x t =
  let rec loop = function
    | Node (l, i, r, _, _) ->
        let rel = cmp x i in
        rel = 0 || loop (if rel < 0 then l else r)
    | Empty -> false in
  loop t

let iter f t =
  let rec loop = function
    | Empty -> ()
    | Node (l, i, r, _, _) -> loop l; f i; loop r in
  loop t

let rec fold f t acc =
  match t with
  | Empty -> acc
  | Node (l, i, r, _, _) ->
        fold f r (f i (fold f l acc))
 
let elements t = 
  let rec loop acc = function
    | Empty -> acc
    | Node(l, i, r, _, _) -> loop (i :: loop acc r) l in
  loop [] t

let below n tt =
  let rec loop acc t =
    match t with
    | Empty -> acc
    | Node (l, (a, b), r, _, _) ->
        let rel = cmp n (a, b) in
        if rel < 0 then loop acc l else
        if rel = 0 then safe_add acc (safe_add (card l) (i_card (a, n))) else
        loop (safe_add acc (safe_add (card l) (i_card (a, b)))) r in
  loop 0 tt 
  