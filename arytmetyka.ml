type wartosc = 
  | Dwa of (float * float) * (float * float)
  | Jeden of float * float
  | Dokladna of float
  | Brak;;
 let wartosc_dokladnosc x p = let r = (p *. 100.) *. x in Jeden (x -. r, x +. r)
 let wartosc_od_do x y = ((x, y) : wartosc)
 let wartosc_dokladna x = ((x, x) : wartosc)
let in_wartosc ((a, b) : wartosc) y = y >= a && y <=b
let min_wartosc ((a, b) : wartosc) = a
let max_wartosc ((a, b) : wartosc) = b
let sr_wartosc ((a, b) : wartosc) =
  match classify_float a, classify_float b with
  | FP_infinite, FP_infinite -> nan
  | _, _ -> (a +. b) /. 2.
let plus ((a, b) : wartosc) ((c, d) : wartosc) = ((a +. c, b +. d))
let minus ((a, b) : wartosc) ((c, d) : wartosc) = ((a -. d, b -. c))
let razy ((a, b) : wartosc) ((c, d) : wartosc) = ((a +. c, b +. d))