module Vol = Volmodule Vol = Vol
module Parking = Parking
module Ttot = Ttot

let nb_conflits_parking_par_heure (vols : Vol.t list) =
    let heures = Array.make 24 0 in
    List.iter (fun (vol : Vol.t) ->
      let heure = vol.etot / 3600 in
      if vol.conflit_parking then
        heures.(heure) <- heures.(heure) + 1
    ) vols;
    heures


let ecrire_statistiques_par_heure_csv (vols : Vol.t list) (nom_fichier : string) =
    let nb_vols = nb_vols_par_heure vols in
    let nb_conflits_parking = nb_conflits_parking_par_heure vols in
    let nb_creneaux_rates = nb_creneaux_rates_par_heure vols in
    let heures = Array.make 24 (0, 0, 0, 0) in
    List.iter (fun (vol : Vol.t) ->
      let heure = vol.etot / 3600 in
      let (total_retard, count, _, _) = heures.(heure) in
      heures.(heure) <- (total_retard + (vol.ttot - vol.etot), count + 1, nb_conflits_parking.(heure), nb_creneaux_rates.(heure))
    ) vols;
    let fichier = open_out nom_fichier in
    fprintf fichier "Heure,NB_vols,Retard moyen (minutes),Nb_conflits_parking,Nb_creneaux_rates\n";
    Array.iteri (fun heure (total_retard, count, conflits_parking, creneaux_rates) ->
      if count > 0 then
        fprintf fichier "%02d,%d,%.2f,%d,%d\n" heure nb_vols.(heure) (float_of_int total_retard /. float_of_int count /. 60.0) conflits_parking creneaux_rates
      else
        fprintf fichier "%02d,%d,Aucun vol,%d,%d\n" heure nb_vols.(heure) conflits_parking creneaux_rates
    ) heures;
    close_out fichier

let nb_conflits_parking_par_heure (vols : Vol.t list) =
    let heures = Array.make 24 0 in
    List.iter (fun (vol : Vol.t) ->
      let heure = vol.etot / 3600 in
      if vol.conflit_parking then
        heures.(heure) <- heures.(heure) + 1
    ) vols;
    heures

let nb_creneaux_rates_par_heure (vols : Vol.t list) =
    let heures = Array.make 24 0 in
    List.iter (fun (vol : Vol.t) ->
      let heure = vol.etot / 3600 in
      if vol.ttot > vol.etot + 600 then
        heures.(heure) <- heures.(heure) + 1
    ) vols;
    heures

let ecrire_statistiques_par_heure_csv (vols : Vol.t list) (nom_fichier : string) =
    let nb_vols = nb_vols_par_heure vols in
    let nb_conflits_parking = nb_conflits_parking_par_heure vols in
    let nb_creneaux_rates = nb_creneaux_rates_par_heure vols in
    let heures = Array.make 24 (0, 0, 0, 0) in
    List.iter (fun (vol : Vol.t) ->
      let heure = vol.etot / 3600 in
      let (total_retard, count, _, _) = heures.(heure) in
      heures.(heure) <- (total_retard + (vol.ttot - vol.etot), count + 1, nb_conflits_parking.(heure), nb_creneaux_rates.(heure))
    ) vols;
    let fichier = open_out nom_fichier in
    fprintf fichier "Heure,NB_vols,Retard moyen (minutes),Nb_conflits_parking,Nb_creneaux_rates\n";
    Array.iteri (fun heure (total_retard, count, conflits_parking, creneaux_rates) ->
      if count > 0 then
        fprintf fichier "%02d,%d,%.2f,%d,%d\n" heure nb_vols.(heure) (float_of_int total_retard /. float_of_int count /. 60.0) conflits_parking creneaux_rates
      else
        fprintf fichier "%02d,%d,Aucun vol,%d,%d\n" heure nb_vols.(heure) conflits_parking creneaux_rates
    ) heures;
    close_out fichier