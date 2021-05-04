(* Implementacja przelewanka.mli *)
(* Autor: Tomasz Ziebowicz       *)

let przelewanka tab =
  let n = Array.length tab
  (* Funkcja sprawdza, czy każdy stan końcowy jest podzielny przez nwd stanów
     przez nwd pojemności szklanek. Jeśli tak nie jest, to nie da się
     doprowadzić do stanu końcowego.

     Złożoność czasowa: O(nlogn)
     Złożoność pamięciowa: O(1)
  *)
  in let heurystyka_nwd =  
    let rec nwd a b =
      if b = 0 then a
      else nwd b (a mod b)
    in let x_nwd = Array.fold_left (fun a (x_i, _) -> nwd a x_i) 0 tab
    in not (x_nwd = 0) && Array.for_all (fun (_, y_i) -> y_i mod x_nwd = 0) tab
  (*
    Funkcja sprawdza, czy istnieje końcowy stan szklanki, który jest
    stanem pełnym lub pustym. Jeśli tak nie jest, to nie da się doprowadzić
    do stanu końcowego.

    Złożoność czasowa: O(n)
    Złożoność pamięciowa: O(1)
  *)
  and heurystyka_pusty_pelny =
    Array.exists (fun (x_i, y_i) -> x_i = y_i || y_i = 0) tab
  in
    if Array.for_all (fun (_, y_i) -> y_i = 0) tab then 0 
    else if not (heurystyka_nwd) || (not heurystyka_pusty_pelny) then -1 
    else
  (*
    Następne instrukcje wykonują algorytm BFS na grafie stanów możliwych do 
    osiągnięcia przez wykonywanie 3 operacji opisanych w poleceniu 
    - nalej, przelej, wylej.

    Wierzchołek startowy to stan początkowy - wszystkie szklanki puste.

    Jeśli sąsiad rozwazanego wierzchołka jest stanem końcowym, nie dochodzi
    do kolejnej iteracji pętli, a funkcja zwraca odległość między wierzchołkiem
    startowym, a znalezionym wierzchołkiem reprezentującym stan końcowy.

    Wierzchołki dodajemy do kolejki w postaci:
    (Tablica_stanu, Liczba_szklanek_nie_w_stanie_docelowym).
    
    Złożoność czasowa: O(Liczba_możliwych_stanów * Liczba_szklanek^2)
    Złożoność pamięciowa: O(Liczba_możliwych_stanów) 
  *)
      let cel i = snd tab.(i)
      and poj i = fst tab.(i)
      (*
      Tablica z haszowaniem przechowująca długość najkrótszej ścieżki
      z wierzchołka początkowego do danego wierzchołka.
      *)
      and htbl = Hashtbl.create (n * n)
      and q = Queue.create ()
      and (pocz_s, pocz_poz) = (Array.make n 0, Array.fold_left 
        (fun a (_, y_i) -> if y_i <> 0 then a + 1 else a) 0 tab)
      in let znaleziono = ref false
      and odp = ref (-1)
      in let odw s d =
        Hashtbl.add htbl s d
      (*
      -1 to wartość specjalna, przekazuje informację, że wierzchołek s nie był
      wcześniej napotkany.
      *)
      and dys s =
        try Hashtbl.find htbl s
        with Not_found -> -1
      (*
      Funkcja generująca sąsiadów wierzchołka s - stany osiągalne
      po wykonaniu jednej z operacji na stanie s.

      Złożoność czasowa: O(Liczba_szklanek^2)
      Złożoność pamięciowa: O(Liczba_szklanek^2) 
      *)
      in let sasiedzi (s, poz) =
        let nalej i w =
          let ns = Array.copy s in begin
            ns.(i) <- poj i;
            if dys ns = -1 then
              let npoz = if w = cel i then -1 else 0
              in let npoz = if poj i = cel i then npoz + 1 else npoz
              in let npoz = poz - npoz
              and ndys = (dys s) + 1 
              in begin
                Queue.push (ns, npoz) q;
                odw ns ndys;
                if npoz = 0 then begin
                  znaleziono := true;
                  odp := ndys
                end
              end
          end
        and wylej i w =
          let ns = Array.copy s in begin
            ns.(i) <- 0;
            if dys ns = -1 then
              let npoz = if w = cel i then -1 else 0
              in let npoz = if cel i = 0 then npoz + 1 else npoz
              in let npoz = poz - npoz
              and ndys = (dys s) + 1 
              in begin
                Queue.push (ns, npoz) q;
                odw ns ndys;
                if npoz = 0 then begin
                  znaleziono := true;
                  odp := ndys
                end
              end
          end
        and przelej i w =
            let z_do i j wj =
              let ns = Array.copy s
              and mie = (poj j) - wj
              in begin
                ns.(i) <- max 0 (w - mie);
                ns.(j) <- min (poj j) (wj + w);
                if dys ns = -1 then
                let npoz = if w = cel i then -1 else 0
                in let npoz = if wj = cel j then npoz - 1 else npoz
                in let npoz = if ns.(i) = cel i then npoz + 1 else npoz
                in let npoz = if ns.(j) = cel j then npoz + 1 else npoz
                in let npoz = poz - npoz
                and ndys = (dys s) + 1 
                in begin
                  Queue.push (ns, npoz) q;
                  odw ns ndys;
                  if npoz = 0 then begin
                    znaleziono := true;
                    odp := ndys
                  end
                end
              end
            in Array.iteri (fun j wj ->
              if j <> i then z_do i j wj) s
        in begin
          Array.iteri nalej s;
          Array.iteri wylej s;
          Array.iteri przelej s
        end
      in
        odw pocz_s 0;
        Queue.push (pocz_s, pocz_poz) q;
        while not (Queue.is_empty q) && not !znaleziono do
          let s = Queue.pop q
          in sasiedzi s
        done;
        !odp
        


        
        
        

  
  
