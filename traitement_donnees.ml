module Vol = Vol
module Parking = Parking

let nb_creneaux_rates_par_heure (vols : Vol.t list) =
  let heures = Array.make 24 0 in
  List.iter (fun (vol : Vol.t) ->
    if vol.ttot > vol.etot + 600 && vol.type_vol = "DEP" then 
      let heure = vol.etot / 3600 in
      heures.(heure) <- heures.(heure) + 1
  ) vols;
  heures

let nb_vols_par_heure (vols : Vol.t list) =
  let heures = Array.make 24 0 in
  List.iter (fun (vol : Vol.t) ->
    if vol.type_vol = "DEP" then
      let heure = vol.etot / 3600 in
      heures.(heure) <- heures.(heure) + 1
    else if vol.type_vol = "ARR" then
      let heure_debut = int_of_string vol.heure_debut in
      let heure = heure_debut / 3600 in
      heures.(heure) <- heures.(heure) + 1
  ) vols;
  heures

let nb_vols_par_parking (hashtbl_parkings : Vol.t list Parking.ParkingHashtbl.t) =
  let parkings = Hashtbl.create (Parking.ParkingHashtbl.length hashtbl_parkings) in
  Parking.ParkingHashtbl.iter (fun parking vols_parking ->
    Hashtbl.add parkings parking (List.length vols_parking)
  ) hashtbl_parkings;
  parkings

  let nb_depart_arrivee_par_heure (vols : Vol.t list) =
    let heures = Array.make 24 (0,0) in
    List.iter (fun (vol : Vol.t) ->
      let heure = 
        if vol.type_vol = "DEP" then vol.etot / 3600
        else int_of_string vol.heure_debut / 3600
      in
      let (deps, arrs) = heures.(heure) in
      if vol.type_vol = "DEP" then 
        heures.(heure) <- (deps + 1, arrs)
      else if vol.type_vol = "ARR" then
        heures.(heure) <- (deps, arrs + 1)
    ) vols;
    heures

let nb_avions_par_piste_par_heure (vols : Vol.t list) =
  let heures = Array.make 24 (0,0,0,0) in (* (26R, 27L, 26L, 27R) *)
  List.iter (fun (vol : Vol.t) ->
    let heure = 
      if vol.type_vol = "DEP" then vol.etot / 3600
      else int_of_string vol.heure_debut / 3600
    in
    let (p26r, p27l, p26l, p27r) = heures.(heure) in
    match vol.qfu with
    | "26R" -> heures.(heure) <- (p26r + 1, p27l, p26l, p27r)
    | "27L" -> heures.(heure) <- (p26r, p27l + 1, p26l, p27r)
    | "26L" -> heures.(heure) <- (p26r, p27l, p26l + 1, p27r)
    | "27R" -> heures.(heure) <- (p26r, p27l, p26l, p27r + 1)
    | _ -> ()
  ) vols;
  heures

let ecrire_statistiques_par_heure_csv (vols : Vol.t list) (hashtbl_parkings : Vol.t list Parking.ParkingHashtbl.t) (nom_fichier : string) =
  let nb_vols = nb_vols_par_heure vols in
  let nb_creneaux_rates = nb_creneaux_rates_par_heure vols in
  let heures = Array.make 24 (0, 0, 0, 0) in
  let nb_dep_arr = nb_depart_arrivee_par_heure vols in
  let nb_par_piste = nb_avions_par_piste_par_heure vols in
  
  (* Calcul des retards et conflits pour chaque heure *)
  List.iter (fun (vol : Vol.t) ->
    let heure = vol.etot / 3600 in
    let (total_retard, count, _, _) = heures.(heure) in
    heures.(heure) <- (total_retard + (vol.ttot - vol.etot), count + 1, 0, nb_creneaux_rates.(heure))
  ) vols;
  
  (* Calcul des conflits de parking *)
  Parking.ParkingHashtbl.iter (fun _ vols_parking ->
    List.iter (fun (vol : Vol.t) ->
      let heure = vol.etot / 3600 in
      let (total_retard, count, conflits_parking, creneaux_rates) = heures.(heure) in
      let total_conflicts = Parking.count_conflicts vols_parking in
      heures.(heure) <- (total_retard, count, conflits_parking + total_conflicts, creneaux_rates)
    ) vols_parking
  ) hashtbl_parkings;
  
  let fichier = open_out nom_fichier in
  Printf.fprintf fichier "Heure,NB_vols,Retard moyen (minutes),Retard total (minutes),Nb_conflits_parking,Nb_creneaux_rates,NB_Departs,NB_Arrivees,Piste_26R,Piste_27L,Piste_26L,Piste_27R\n";
  
  Array.iteri (fun heure (total_retard, count, conflits_parking, creneaux_rates) ->
    let (deps, arrs) = nb_dep_arr.(heure) in
    let (p26r, p27l, p26l, p27r) = nb_par_piste.(heure) in
    if count > 0 then
      Printf.fprintf fichier "%02d,%d,%.2f,%d,%d,%d,%d,%d,%d,%d,%d,%d\n" 
        heure nb_vols.(heure) (float_of_int total_retard /. float_of_int count /. 60.0) 
        (total_retard / 60) conflits_parking creneaux_rates deps arrs
        p26r p27l p26l p27r
    else
      Printf.fprintf fichier "%02d,%d,Aucun vol,0,%d,%d,%d,%d,%d,%d,%d,%d\n" 
        heure nb_vols.(heure) conflits_parking creneaux_rates deps arrs
        p26r p27l p26l p27r
  ) heures;
  close_out fichier