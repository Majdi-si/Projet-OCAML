module Vol = Vol
module Parking = Parking
module Optimisation = Optimisation

type config_csv = {
  nom_fichier : string;
  avec_optimisation : bool;
  afficher_conflits : bool
}

let nb_creneaux_rates_par_heure (vols : Vol.t list) =
  let heures = Array.make 24 0 in
  List.iter (fun (vol : Vol.t) ->
    if vol.type_vol = "DEP" then 
      let heure = vol.etot / 3600 in
      if vol.creneau <> "_" then (
        (* Si le vol a un créneau NMOC *)
        let creneau = int_of_string vol.creneau in
        if vol.ttot > creneau + 600 || vol.ttot < creneau - 300 then
          heures.(heure) <- heures.(heure) + 1
      ) else (
        (* Sinon on vérifie par rapport à l'ETOT *)
        if vol.ttot > vol.etot + 600 then
          heures.(heure) <- heures.(heure) + 1
      )
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

let ecrire_statistiques_par_heure_csv (vols : Vol.t list) 
    (hashtbl_parkings : Vol.t list Parking.ParkingHashtbl.t) 
    (config : config_csv) =

  List.iter (fun vol ->
    vol.Vol.occupation_parking <- { lower = 0; upper = 0 }
  ) vols;
  Parking.calculer_intervalles_occupation vols;
  Parking.tri_heure_debut vols hashtbl_parkings;

  let nb_vols = nb_vols_par_heure vols in
  let heures = Array.make 24 (0, 0, 0, 0) in
  let nb_dep_arr = nb_depart_arrivee_par_heure vols in
  let nb_par_piste = nb_avions_par_piste_par_heure vols in

  (* Calcul des retards par heure *)
  List.iter (fun vol ->
    if vol.Vol.type_vol = "DEP" then
      let heure = vol.Vol.etot / 3600 in
      let (total_retard, count, conflits, rates) = heures.(heure) in
      let retard = vol.Vol.ttot - vol.Vol.etot in
      if retard > 0 then
        heures.(heure) <- (total_retard + retard, count + 1, conflits, rates)
  ) vols;

  (* Calcul des conflits de parking avec regroupement par heure *)
  let conflits_par_heure = Array.make 24 0 in
  Parking.ParkingHashtbl.iter (fun parking vols_parking ->
    List.iter (fun vol1 ->
      let heure1 = 
        if vol1.Vol.type_vol = "DEP" then vol1.Vol.dman / 3600
        else vol1.Vol.heure_parking / 3600
      in
      List.iter (fun vol2 ->
        if vol1 != vol2 && vol1.Vol.parking = vol2.Vol.parking then
          let heure2 = 
            if vol2.Vol.type_vol = "DEP" then vol2.Vol.dman / 3600
            else vol2.Vol.heure_parking / 3600
          in
          if heure1 = heure2 && 
            vol1.Vol.occupation_parking.lower < vol2.Vol.occupation_parking.upper &&
            vol1.Vol.occupation_parking.upper > vol2.Vol.occupation_parking.lower then
            conflits_par_heure.(heure1) <- conflits_par_heure.(heure1) + 1
      ) vols_parking
    ) vols_parking
  ) hashtbl_parkings;

  (* Diviser les conflits par 2 car chaque conflit est compté deux fois *)
  Array.iteri (fun i conflicts ->
    conflits_par_heure.(i) <- conflicts / 2
  ) conflits_par_heure;

  (* Mettre à jour les statistiques *)
  Array.iteri (fun i (total_retard, count, _, rates) ->
    heures.(i) <- (total_retard, count, conflits_par_heure.(i), rates)
  ) heures;

  let retards_optimises = 
    if config.avec_optimisation then begin
      (* Optimiser les pistes 26R et 27L *)
      let vols_26R = List.filter (fun v -> v.Vol.type_vol = "DEP" && v.Vol.qfu = "26R") vols in
      let vols_27L = List.filter (fun v -> v.Vol.type_vol = "DEP" && v.Vol.qfu = "27L") vols in
      
      let resultats_26R = Array.make 24 0.0 in
      let resultats_27L = Array.make 24 0.0 in
      
      (* Optimiser chaque heure pour 26R *)
      for h = 0 to 23 do
        let vols_heure = List.filter (fun v -> v.Vol.etot / 3600 = h) vols_26R in
        if List.length vols_heure > 0 then begin
          let sequence = Optimisation.optimiser_sequence vols_heure in
          resultats_26R.(h) <- 
            float_of_int sequence.cout /. float_of_int (List.length sequence.vols) /. 60.0
        end
      done;

      (* Optimiser chaque heure pour 27L *)
      for h = 0 to 23 do
        let vols_heure = List.filter (fun v -> v.Vol.etot / 3600 = h) vols_27L in
        if List.length vols_heure > 0 then begin
          let sequence = Optimisation.optimiser_sequence vols_heure in
          resultats_27L.(h) <- 
            float_of_int sequence.cout /. float_of_int (List.length sequence.vols) /. 60.0
        end
      done;

      Some (resultats_26R, resultats_27L)
    end else None in

  (* Écriture dans le fichier *)
  let fichier = open_out config.nom_fichier in
  
  (* En-tête *)
  let header = "Heure;NB_vols;Retard moyen (minutes);Retard total (minutes);Nb_vols_en_retard" ^
  (if config.afficher_conflits then ";Nb_conflits_parking" else "") ^
  ";Nb_creneaux_rates;NB_Departs;NB_Arrivees;Piste_26R;Piste_27L;Piste_26L;Piste_27R" ^
  (if config.avec_optimisation then ";Retard_opt_26R;Retard_opt_27L;Retard_opt_cumule;Retard_total_opt" else "") ^
  "\n" in
  output_string fichier header;
  
  (* Écriture des données *)
  Array.iteri (fun heure (total_retard, count, conflits_parking, creneaux_rates) ->
    let (deps, arrs) = nb_dep_arr.(heure) in
    let (p26r, p27l, p26l, p27r) = nb_par_piste.(heure) in
  
    (* Calcul du retard moyen - utilise count pour ne compter que les vols avec retard *)
    let retard_moyen = if count > 0 then 
      Printf.sprintf "%.2f" (float_of_int total_retard /. float_of_int count /. 60.0)
      |> String.map (function '.' -> ',' | c -> c)
    else "0,00" in
    
    (* Calcul du retard total - conversion directe des secondes en minutes *)
    let retard_total = if total_retard > 0 then
      Printf.sprintf "%.2f" (float_of_int total_retard /. 60.0)
      |> String.map (function '.' -> ',' | c -> c)
    else "0,00" in
  
    (* Calcul des retards optimisés *)
    let (r26R, r27L, retard_cumule, retard_total_opt) = match retards_optimises with
    | Some (r26R, r27L) -> 
      let r26R_val = if r26R.(heure) > 0.0 then r26R.(heure) else 0.0 in
      let r27L_val = if r27L.(heure) > 0.0 then r27L.(heure) else 0.0 in
      let cumule = r26R_val +. r27L_val in
      let total = cumule *. float_of_int count in
      (Printf.sprintf "%.2f" r26R_val |> String.map (function '.' -> ',' | c -> c),
       Printf.sprintf "%.2f" r27L_val |> String.map (function '.' -> ',' | c -> c),
       Printf.sprintf "%.2f" cumule |> String.map (function '.' -> ',' | c -> c),
       Printf.sprintf "%.2f" total |> String.map (function '.' -> ',' | c -> c))
  | None -> ("0,00", "0,00", "0,00", "0,00")
in
  let conflits_str = if config.afficher_conflits then string_of_int conflits_parking else "0" in
let line = Printf.sprintf "%02d;%d;%s;%s;%d;%s;%d;%d;%d;%d;%d;%d;%d;%s;%s;%s;%s\n"
  heure 
  nb_vols.(heure)
  retard_moyen
  retard_total
  count
  conflits_str
  creneaux_rates 
  deps arrs 
  p26r p27l p26l p27r
  r26R r27L 
  retard_cumule 
  retard_total_opt 
in
output_string fichier line
  ) heures;
  close_out fichier