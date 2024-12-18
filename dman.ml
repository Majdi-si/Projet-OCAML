module Vol = Vol
module TTOT= Ttot

let calculer_dman (vols : Vol.t list) : Vol.t list =
  List.map (fun (vol : Vol.t) -> 
    let dman = 
        let ttot = vol.ttot in
        let nombre_points = List.length vol.points in
        ttot - (5 * nombre_points) in
    { vol with dman }   
  ) vols

let calculer_heure_parking (vols : Vol.t list) : Vol.t list =
  List.map (fun (vol : Vol.t) -> 
    let heure_parking = 
        let heure_piste_int = int_of_string vol.heure_piste in
        let nombre_points = List.length vol.points in
        heure_debut_int + (5 * nombre_points)
    in
    { vol with heure_parking }   
  ) vols

let nbre_creneaux_rates (vols : Vol.t list) : Vol.t list =
  let rec creneau_rec acc (vols : Vol.t list) =
    match vols with
    [] ->  acc
    |vol::reste -> 
      if vol.ttot > vol.creneau + 600 then  creneau_rec (acc + 1) reste
      else   creneau_rec acc reste in
  creneau_rec 0 vols
