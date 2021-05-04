(* Implementacja topol.mli *)
(* Autor: Tomasz Ziebowicz *)

open PMap;;

exception Cykliczne

type stan = Odwiedzone | Przetworzone | Nieodwiedzone

(*
Funkcja zmapuj: 
  Konstruuje mapę reprezentującą graf zadany przez listę podaną tej funkcji.
  Wierzchołki są reprezentowane w postaci (lista sąsiadów, stan).

Funkcja zmien_stan: 
  Zwraca tak zmodyfikowaną mapę g, że stan wierzchołka v jest równy s.

Funkcja zmien_sasiadow: 
  Zwraca tak zmodyfikowaną mapę g, że lista_sąsiadów wierzchołka v jest
  zaktualizowana o wierzchołki w liście n.

Funkcja dfs:
  Konstruuje liste posortowanych topologicznie wierzchołków, do których da się
  dotrzeć z wierzchołka, którego listę sąsiadów ta funckja otrzymuje. Funkcja
  działa w oparciu o algorytm "Depth-first search".

Złożoność czasowa funkcji topol jest rzędu O(V + E), gdzie E to ilość krawędzi
opisanych w liście wejściowej, a V to ilość zawartych w niej różnych wierzchołków.
*)
let topol graf =
  let rec zmapuj g = function
  | [] -> g
  | (v, n)::t -> 
     zmapuj ((if not (exists v g) then add v (n, Nieodwiedzone) 
              else zmien_sasiadow v n)
               (List.fold_left (fun ng sv ->
                    if not (exists sv ng) then
                      add sv ([], Nieodwiedzone) ng 
                    else ng)
                  g n)) t
  and stan v g =
    find v g |> snd
  and sasiedzi v g =
    find v g |> fst
  and zmien_stan v s g =
    add v (sasiedzi v g, s) g
  and zmien_sasiadow v n g =
    add v (n @ (sasiedzi v g), Nieodwiedzone) g
  and dfs g w = 
    let wejdz (ng, wyn) s =
      match stan s ng with
      | Odwiedzone -> raise Cykliczne
      | Przetworzone -> (ng, wyn)
      | Nieodwiedzone ->
           let (nng, nwyn) = 
             dfs (zmien_stan s Odwiedzone ng) wyn (sasiedzi s ng)
           in (zmien_stan s Przetworzone nng, s::nwyn)
    in List.fold_left wejdz (g, w)
  in let ngraf = zmapuj empty graf
  and wejdz v _ (g, wyn) =
    match stan v g with
    | Przetworzone | Odwiedzone -> (g, wyn)
    | Nieodwiedzone ->
       let (ng, nwyn) = 
           dfs (zmien_stan v Odwiedzone g) wyn (sasiedzi v g)
       in (zmien_stan v Przetworzone ng, v::nwyn)
  in foldi wejdz ngraf (ngraf, []) |> snd
    

