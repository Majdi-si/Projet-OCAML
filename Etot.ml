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

  (* fonction de filtrage des departs par qfu*)
  let rec dep_qfu = fun l piste ->
  
  match l with 
  [] -> []
  |t::reste ->
  if t.type_vol= "DEP" && t.qfu= piste 
    then t:: dep_qfu reste piste
  else dep_qfu reste piste

(* Fonction d' insertion *)
let rec insere = fun elem liste ->
 match liste with
|  [] -> elem::[]
|  tete::queue ->
   if elem.etot < tete.etot then elem :: liste
   else tete :: insere elem queue

(* Fonction de tri par insertion *)
let dep_sort l =
  let rec dep_sort_rec l acc =
    match l with
    | [] -> acc  
    | tete :: queue -> 
         dep_sort_rec queue (insere tete acc)  
  in
   dep_sort_rec l []  
