
(* Implementacja origami.mli *)
(* Author: Tomasz Ziebowicz  *)


type point = float * float

type kartka = point -> int

(* Porównywanie z dokładnością do epsilon*)
let epsilon = 1e-9;;

let (<.) a b = a +. epsilon < b

let (=.) a b = a +. epsilon >= b && a -. epsilon <= b

let (<=.) a b = a =. b || a <. b

(* Odległość między 2 punktami do kwadratu. *)
let odl_2 (ax, ay) (bx, by) =
  ((ax -. bx) ** 2.) +. ((ay -. by) ** 2.)

let prostokat (p1x, p1y) (p2x, p2y) = function (px, py) ->
  if p1x <=. px && p1y <=. py && px <=. p2x && py <=. p2y then 1 else 0

let kolko s r = function p ->
  if sqrt (odl_2 s p) <=. r then 1 else 0

let wektor (p1x, p1y) (p2x, p2y) = 
  (p2x -. p1x, p2y -. p1y)

(* Funkcja zwraca iloczyn wektorowy wektorów "p1p2" i "p1p" *)
let ilo_wek p1 p2 p = 
  let (v1x, v1y) = wektor p1 p2 and
  (v2x, v2y) = wektor p1 p in
  (v1x *. v2y) -. (v1y *. v2x)

(* Funkcja zwraca prarę (a, b), gdzie y = ax + b jest prostą zadaną przez
   punkty (p1x, p1y) (p2x, p2y). Jeśli ta prosta jest równoległa do osi OY,
   postaci x = b, to zwracana jest szczególna wartość (infinity, b). *)
let prosta (p1x, p1y) (p2x, p2y) =
  if p1x = p2x then (infinity, p1x) else
  let (x, y) = wektor (p1x, p1y) (p2x, p2y) in
  let a = y /. x in
  (a, p1y -. (a *. p1x))

(* Funkcja zwraca współrzędne punktu symetrycznego do (x, y) względem prostej
   zadanej w postaci (a, b). *)
let symetria (a, b) (x, y) = 
  if a = infinity then ((2. *. b) -. x, y) else
  if a = 0. then (x, 2. *. b -. y) else
  let (a2, b2) = (~-. (1. /. a), y -. ((~-. (1. /. a)) *. x)) in
  let nx = (b2 -. b) /. (a -. a2) in
  let nx = (2. *. nx) -. x in
  ((nx, (a2 *. nx) +. b2) : point)

let zloz p1 p2 k p =
  let ilo = ilo_wek p1 p2 p in
  if ilo <. 0. then 0 else
  if ilo =. 0. then k p else
  (k p) + (k (symetria (prosta p1 p2) p))

let skladaj lista k =
  List.fold_left (fun kartka (p1, p2) -> zloz p1 p2 kartka) k lista