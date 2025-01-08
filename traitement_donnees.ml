module Vol = Vol
module Parking = Parking

let nb_creneaux_rates_par_heure (vols : Vol.t list) =
  let heures = Array.make 24 0 in
  List.iter (fun (vol : Vol.t) ->
    (*Printf.printf "Vol %d %d\n" vol.etot vol.ttot;*)
    if vol.ttot > vol.etot + 600 && vol.type_vol = "DEP" then 
    let heure = vol.etot / 3600 in
      heures.(heure) <- heures.(heure) + 1;
      (*Printf.printf "Vol %s rate le créneau horaire\n" vol.indicatif*)
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

let ecrire_statistiques_par_heure_csv (vols : Vol.t list) (hashtbl_parkings : Vol.t list Parking.ParkingHashtbl.t) (nom_fichier : string)  =
  let nb_vols = nb_vols_par_heure vols in
  let nb_creneaux_rates = nb_creneaux_rates_par_heure vols in
  let heures = Array.make 24 (0, 0, 0, 0) in
  List.iter (fun (vol : Vol.t) ->
    let heure = vol.etot / 3600 in
    let (total_retard, count, _, _) = heures.(heure) in
    heures.(heure) <- (total_retard + (vol.ttot - vol.etot), count + 1, 0, nb_creneaux_rates.(heure))
  ) vols;
  
  (* Mettez à jour les conflits de parking dans le tableau heures *)
  Parking.ParkingHashtbl.iter (fun _ vols_parking ->
    List.iter (fun (vol : Vol.t) ->
      let heure = vol.etot / 3600 in
      let (total_retard, count, conflits_parking, creneaux_rates) = heures.(heure) in
      let total_conflicts = Parking.count_conflicts vols_parking in
      heures.(heure) <- (total_retard, count, conflits_parking + total_conflicts, creneaux_rates)
    ) vols_parking
  ) hashtbl_parkings;
  
  let fichier = open_out nom_fichier in
  Printf.fprintf fichier "Heure,NB_vols,Retard moyen (minutes),Retard total (minutes),Nb_conflits_parking,Nb_creneaux_rates\n";
  Array.iteri (fun heure (total_retard, count, conflits_parking, creneaux_rates) ->
    if count > 0 then
      Printf.fprintf fichier "%02d,%d,%.2f,%d,%d,%d\n" heure nb_vols.(heure) (float_of_int total_retard /. float_of_int count /. 60.0) (total_retard / 60) conflits_parking creneaux_rates
    else
      Printf.fprintf fichier "%02d,%d,Aucun vol,0,%d,%d\n" heure nb_vols.(heure) conflits_parking creneaux_rates
  ) heures;
  close_out fichier
