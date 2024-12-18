(* Etot.ml *)
module Vol = Vol

let calculer_etot (vols : Vol.t list) : Vol.t list =
  List.map (fun (vol : Vol.t) -> 
    let etot = 
      if vol.type_vol = "DEP" && vol.creneau = "_" then
        let heure_debut_int = int_of_string vol.heure_debut in
        let nombre_points = List.length vol.points in
        heure_debut_int + (5 * nombre_points)
      else if vol.type_vol = "DEP" then 
        int_of_string vol.creneau
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
  let rec dep_qfu (l : Vol.t list) (piste : string) : Vol.t list =
    match l with 
    [] -> []
    |t::reste ->
      if t.type_vol= "DEP" && t.qfu= piste 
        then t:: dep_qfu reste piste
      else dep_qfu reste piste


  (* fonction de filtrage des arrivée par qfu*)
  let rec arr_qfu (l : Vol.t list) (piste : string) : Vol.t list =
    match l with 
    [] -> []
    |t::reste ->
      if t.type_vol= "ARR" && t.qfu= piste 
        then t:: arr_qfu reste piste
      else arr_qfu reste piste

(* Fonction d' insertion *)
let rec insere (elem : Vol.t) (liste : Vol.t list) : Vol.t list =
  match liste with
  [] -> elem::[]
  |  tete::queue ->
     if elem.etot < tete.etot then elem :: liste
    else tete :: insere elem queue


(*Trier les vols par ETOT*)
let tri_etot (l : Vol.t list) : Vol.t list =
  let rec tri_etot_rec l acc =
    match l with
    | [] -> acc
    | tete :: queue -> 
      tri_etot_rec queue (insere tete acc)
  in
  tri_etot_rec l []




  