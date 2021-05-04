(* Implementacja arytmetyka.mli *)
(* Autor: Tomasz Ziebowicz      *)

(* ----- Definicje typów ----- *)

(*
 * Przedziały niepuste traktuję jako domknięte. 
 * Dla każdego przedziału Niepusty(a, b), a <= b.
 *)
type przedzial = 
| Niepusty of float * float
| Pusty

(* 
 * Traktuję typ wartość jako sumę dwóch przedziałów. 
 *)
type wartosc = przedzial * przedzial

(* ----- Konstruktory ----- *)

let wartosc (a : przedzial) (b: przedzial) : wartosc = (a, b)

let wartosc_dokladnosc x p = 
  let r = abs_float ((p /. 100.) *. x) 
  in 
  wartosc (Niepusty(x -. r, x +. r)) Pusty

let wartosc_od_do x y = 
  wartosc (Niepusty(x, y)) Pusty

let wartosc_dokladna x = 
  wartosc (Niepusty(x, x)) Pusty

(* ----- Funkcje pomocnicze ----- *)

(* 
 * Większa z dwóch liczb typu float, nan jest najmniejszy. 
 *)
let max a b =
  match classify_float a, classify_float b with
  | FP_nan, FP_nan -> nan
  | FP_nan, _ -> b
  |_, FP_nan -> a
  |_ -> if a < b then b else a

(* 
 * Mniejsza z dwóch liczb typu float, nan jest największy. 
 *)
let min a b =
  match classify_float a, classify_float b with
  | FP_nan, FP_nan -> nan
  | FP_nan, _ -> b
  |_, FP_nan -> a
  |_ -> if a < b then a else b

(* 
 * Największa z czterech liczb typu float, nan jest najmniejszy. 
 *)        
let max4 a b c d =
  max (max (max a b) c) d

(* 
 * Najmniejsza z czterech liczb typu float, nan jest największy. 
 *)
let min4 a b c d =
  min (min (min a b) c) d

(* ----- Selektory ----- *) 

(* 
 * in_przedzial i y = true <=> y należy do przedziału i 
 *)
let in_przedzial (i : przedzial) y =
  match classify_float y with
  | FP_nan when i = Pusty -> true
  | FP_nan -> false
  | _ ->
      match i with
      | Pusty -> false
      | Niepusty(a, b) -> a <= y && y <= b

let in_wartosc ((a, b) : wartosc) y =
  in_przedzial a y || in_przedzial b y
  
let min_wartosc x =
  match x with
  | Pusty, Pusty -> nan
  | Niepusty(a, b), Pusty -> a
  | Pusty, Niepusty(a, b) -> a
  | Niepusty(a, b), Niepusty(c, d) -> min a c

let max_wartosc x =
  match x with
  | Pusty, Pusty -> nan
  | Niepusty(a, b), Pusty -> b
  | Pusty, Niepusty(a, b) -> b
  | Niepusty(a, b), Niepusty(c, d) -> max b d

let sr_wartosc x =
  match x with
  | Pusty, Pusty -> nan
  | _-> 
      let a = min_wartosc x 
      and b = max_wartosc x
      in
      (a +. b) /. 2.

(* ----- Modyfikatory ----- *)

(* 
 * Funckja zwraca uproszczoną w sensie sumowania zbiorów "wartosc" "w". 
 *)
let uprosc_wartosc (w : wartosc) =
  match w with
  | Pusty, Pusty -> w
  | _, Pusty -> w 
  | Pusty, a -> wartosc a Pusty
  | Niepusty(a, b), Niepusty(c, d) ->
      if in_przedzial (Niepusty(a, b)) c 
         || in_przedzial (Niepusty(a, b)) d then
        wartosc (Niepusty(min a c, max b d)) Pusty
      else w

(* 
 * Funckja zwraca uproszczoną w sensie sumowania zbiorów 
 * "wartosc" "w" zsumowaną z przedziałem "i". 
 *)  
let uprosc (w : wartosc) (i : przedzial) =
  match i with
  | Pusty -> w
  | Niepusty(a, b) ->
      match w with
      | Pusty, Pusty -> wartosc (Niepusty(a, b)) Pusty
      | Niepusty(x, y), Pusty ->
          if in_przedzial (Niepusty(x, y)) a 
             || in_przedzial (Niepusty(x, y)) b then
            wartosc (Niepusty(min x a, max y b)) Pusty
          else
            wartosc (Niepusty(x, y)) (Niepusty(a, b))
      | Pusty, Niepusty(x, y) ->
          if in_przedzial (Niepusty(x, y)) a 
             || in_przedzial (Niepusty(x, y)) b then
            wartosc (Niepusty(min x a, max y b)) Pusty
          else  
            wartosc (Niepusty(x, y)) (Niepusty(a, b))
      | Niepusty(x1, y1), Niepusty(x2, y2) ->
          if in_przedzial (Niepusty(x1, y1)) a 
             || in_przedzial (Niepusty(x1, y1)) b then
            wartosc (Niepusty(min x1 a, max y1 b)) (Niepusty(x2, y2))
          else
            wartosc (Niepusty(min x2 a, max y2 b)) (Niepusty(x1, y1))

(* 
 * Funckja zwraca uproszczoną w sensie sumowania zbiorów 
 * sumę elenetów typu "wartosc": "w" oraz "(i1, i2)". 
 *)
let uprosc_wartosci (w : wartosc) ((i1, i2) : wartosc) =
  uprosc (uprosc w i1) i2

(* 
 * plus_przedzial i1 i2 = { x + y : in_przedzial i1 x && in_przedzial i2 y } 
 *)          
let plus_przedzial (i1 : przedzial) (i2 : przedzial) = 
  match i1, i2 with
  | Pusty, _ -> Pusty
  | _, Pusty -> Pusty
  | Niepusty(a, b), Niepusty(c, d) -> Niepusty(a +. c, b +. d)   

let plus ((i1, i2) : wartosc) ((i3, i4) : wartosc) =
  uprosc_wartosci 
  (uprosc_wartosc (wartosc (plus_przedzial i1 i3) (plus_przedzial i1 i4))) 
  (uprosc_wartosc (wartosc (plus_przedzial i2 i3) (plus_przedzial i2 i4)))

(* 
 * minus_przedzial i1 i2 = { x - y : in_przedzial i1 x && in_przedzial i2 y } 
 *) 
let minus_przedzial (i1 : przedzial) (i2 : przedzial) =
  match i2 with
  | Pusty -> Pusty
  | Niepusty(a, b) -> plus_przedzial i1 (Niepusty(~-.b, ~-.a))

let minus ((i1, i2) : wartosc) ((i3, i4) : wartosc) =
  uprosc_wartosci 
  (uprosc_wartosc (wartosc (minus_przedzial i1 i3) (minus_przedzial i1 i4))) 
  (uprosc_wartosc (wartosc (minus_przedzial i2 i3) (minus_przedzial i2 i4)))

(* 
 * razy_przedzial i1 i2 = { x * y : in_przedzial i1 x && in_przedzial i2 y } 
 *) 
let razy_przedzial (i1 : przedzial) (i2 : przedzial) = 
  match i1, i2 with
  | Pusty, _ -> Pusty
  | _, Pusty -> Pusty
  | _, Niepusty(0.,0.) -> Niepusty(0.,0.)
  | Niepusty(0.,0.),_ -> Niepusty(0.,0.)
  | Niepusty(a, b), Niepusty(c, d) ->
      Niepusty(min4 (a *. c) (a *. d) (b *. c) (b *. d), 
               max4 (a *. c) (a *. d) (b *. c) (b *. d))

let razy ((i1, i2) : wartosc) ((i3, i4) : wartosc) =
  uprosc_wartosci 
  (uprosc_wartosc (wartosc (razy_przedzial i1 i3) (razy_przedzial i1 i4))) 
  (uprosc_wartosc (wartosc (razy_przedzial i2 i3) (razy_przedzial i2 i4)))

(* 
 * Funkcja zwraca przedział "i" podzielony na dwa przedziały [a, b] i p 
 * (połączone w "wartosc"), które w sumie są równe "i", takie że:
 *  a <= b = 0 i p jest pusty lub
 *  0 = a <= b i p jest pusty lub
 *  p = [c, d] i a <= b <= 0 <= c <= d. 
 *)
let przetnij_w_0 (i : przedzial) =
  match i with
  | Pusty -> wartosc Pusty Pusty
  | Niepusty(a, b) -> 
      if a <= 0. && 0. <= b then
        match a, b with
        | _, 0. -> wartosc (Niepusty(a, (-0.))) Pusty
        | 0., _ -> wartosc (Niepusty(0., b)) Pusty
        | _ ->  wartosc (Niepusty(a, (-0.))) (Niepusty(0., b))
      else
        wartosc (Niepusty(a, b)) Pusty

(* 
 * podzielic_przedzial i1 i2 = 
 *     { x / y : in_przedzial i1 x && in_przedzial i2 y } 
 *)
let podzielic_przedzial (i1 : przedzial) (i2 : przedzial) = 
  match i1, i2 with
  | _, Niepusty(0.,0.) -> Pusty
  | Pusty, _ -> Pusty
  | _, Pusty -> Pusty
  | Niepusty(a, b), Niepusty(c, d) ->
      Niepusty(min4 (a /. c) (a /. d) (b /. c) (b /. d), 
               max4 (a /. c) (a /. d) (b /. c) (b /. d))

(* 
 * podzielic_przeciete a b = {x / y:  in_wartosc a x && in_wartosc b y }, 
 * dla a i b niezawierających obustronnego otoczenia zera.
 *)
let podzielic_przeciete ((i1, i2) : wartosc) ((i3, i4) : wartosc) =
  uprosc_wartosci 
  (uprosc_wartosc (wartosc (podzielic_przedzial i1 i3) 
                           (podzielic_przedzial i1 i4))) 
  (uprosc_wartosc (wartosc (podzielic_przedzial i2 i3) 
                           (podzielic_przedzial i2 i4)))

let podzielic ((i1, i2) : wartosc) ((i3, i4) : wartosc) =
  uprosc_wartosci (podzielic_przeciete (i1, i2) (przetnij_w_0 i3)) 
                  (podzielic_przeciete (i1, i2) (przetnij_w_0 i4))