(* Etot.ml *)
module Vol = Vol

let calculer_etot (vols : Vol.t list) : Vol.t list =
  List.map (fun (vol : Vol.t) -> 
    let etot = 
      if vol.type_vol = "DEP" then
        let heure_debut_int = int_of_string vol.heure_debut in
        let nombre_points = List.length vol.points in
        heure_debut_int + (5 * nombre_points)
      else
        0
    in
    { vol with etot }   
  ) vols

let afficher_info_etot (vols : Vol.t list) =
  List.iter (fun (vol : Vol.t) ->
    Printf.printf "ETOT: %d\n" vol.etot
  ) vols

  (*
  let rec dep_sort_qfu = fun l piste
  
  match l with 
  [] -> []
  |t::reste ->
  if t.type_vol= "DEP" && t.qfu= piste 
    then t:: (dep_sort reste)
  else dep_sort reste


let rec insere = fun elem liste 
 match liste with
|  [] -> elem::[]
|  tete::queue ->
   if elem < tete then elem :: liste
   else tete :: insere elem queue

let rec dep_sort_qfu = fun l
match l wi


  *)