module Vol = Vol
module Etot = Etot
module Extraire = Extraire
module Ttot = Ttot
module Dman = Dman
module Parking = Parking
module Traitement_donnees = Traitement_donnees
module Optimisation = Optimisation
module Params = Parametre.Params

let optimiser_jusqu_a_heure vols heure_cible piste =
  let dernier_vol = ref None in
  let resultat = ref None in
  
  (* On parcourt toutes les heures jusqu'à l'heure cible *)
  for h = 0 to heure_cible do
    let vols_heure = List.filter (fun v -> 
      let heure = v.Vol.etot / 3600 in
      heure = h && v.Vol.type_vol = "DEP" && v.Vol.qfu = piste
    ) vols in
    
    if List.length vols_heure > 0 then begin
      Printf.printf "\nOptimisation de l'heure %d...\n" h;
      
      (match !dernier_vol with
      | Some last_vol -> 
          List.iter (fun v -> 
            let separation = Ttot.separation_time last_vol.Vol.turbulence v.Vol.turbulence in
            v.Vol.etot <- max v.Vol.etot (last_vol.Vol.ttot + separation)
          ) vols_heure
      | None -> ());

      let sequence = Optimisation.optimiser_sequence vols_heure in
      dernier_vol := Some (List.hd (List.rev sequence.vols));
      
      if h = heure_cible then
        resultat := Some sequence
      else
        Printf.printf "Retard propagé: %d secondes\n" 
          (match !dernier_vol with Some v -> v.Vol.ttot - v.Vol.etot | None -> 0)
    end
  done;
  !resultat


let optimiser_piste vols piste =
  let resultats = Array.make 24 None in
  for h = 0 to 23 do
    let resultat = optimiser_jusqu_a_heure vols h piste in
    resultats.(h) <- resultat
  done;
  resultats

let afficher_resultats_piste piste resultats =
  Printf.printf "\n=== Résultats pour la piste %s ===\n" piste;
  Array.iteri (fun h res ->
    match res with
    | Some sequence -> 
        Printf.printf "\nHeure %d:\n" h;
        Printf.printf "Nombre de départs : %d\n" (List.length sequence.Optimisation.vols);
        Printf.printf "Retard total : %.2f minutes\n" 
          (float_of_int sequence.Optimisation.cout /. 60.0);
        Printf.printf "Retard moyen : %.2f minutes\n" 
          (float_of_int sequence.Optimisation.cout /. float_of_int (List.length sequence.Optimisation.vols) /. 60.0)
    | None -> 
        Printf.printf "\nHeure %d: Aucun vol\n" h
  ) resultats




(* Fonction principale *)
let () =
  let fichier = open_in "data/lfpg_flights.txt" in
  let vols = Vol.extraire_info fichier in
  close_in fichier;

  let parking_time = Params.get_parking_time () in
  let (sep_1, sep_2, sep_3) = Params.get_separation_times () in
  Printf.printf "Temps d'occupation d'un parking: %d minutes\n" (parking_time / 60);
  Printf.printf "Temps de séparation ('M', 'L') | ('H', 'L') | ('H', 'M'): %d minutes\n" (sep_1 / 60);
  Printf.printf "Temps de séparation ('H', 'H'): %d minutes\n" (sep_2 / 60);
  Printf.printf "Temps de séparation pour le reste: %d minutes\n" (sep_3 / 60);


  let vols_avec_etot = Etot.calculer_etot vols in
  let vols_pistes_26R = Etot.dep_qfu vols_avec_etot "26R" in
  let vols_pistes_27L = Etot.dep_qfu vols_avec_etot "27L" in
  let vols_pistes_26L = Etot.arr_qfu vols_avec_etot "26L" in
  let vols_pistes_27R = Etot.arr_qfu vols_avec_etot "27R" in
  let tri_etot_26R = Etot.tri_etot vols_pistes_26R in
  let tri_etot_27L = Etot.tri_etot vols_pistes_27L in
  let tri_etot_26L = Etot.tri_etot vols_pistes_26L in
  let tri_etot_27R = Etot.tri_etot vols_pistes_27R in
  let ttot_26R = Ttot.calculer_ttot tri_etot_26R in
  let ttot_27L = Ttot.calculer_ttot tri_etot_27L in
  let dman_26R = Dman.calculer_dman ttot_26R in
  let dman_27L = Dman.calculer_dman ttot_27L in
  let dman_26L = Dman.calculer_dman tri_etot_26L in
  let dman_27R = Dman.calculer_dman tri_etot_27R in
  let heure_parking_26R = Dman.calculer_heure_parking dman_26R in
  let heure_parking_27L = Dman.calculer_heure_parking dman_27L in
  let heure_parking_26L = Dman.calculer_heure_parking dman_26L in
  let heure_parking_27R = Dman.calculer_heure_parking dman_27R in
  let nombre_parkings_differents = Parking.nombre_parkings_differents vols in
  let liste_parking_differents = Parking.liste_parking_differents vols in
  let hashtbl_parkings = Parking.create_hashtbl_vide nombre_parkings_differents in
  Parking.remplir_hashtbl heure_parking_26L hashtbl_parkings;
  Parking.remplir_hashtbl heure_parking_26R hashtbl_parkings;
  Parking.remplir_hashtbl heure_parking_27L hashtbl_parkings;
  Parking.remplir_hashtbl heure_parking_27R hashtbl_parkings;
  (*Parking.afficher_hashtbl hashtbl_parkings;*)
  let tous_vols = heure_parking_26L @ heure_parking_26R @ heure_parking_27L @ heure_parking_27R in
  Parking.calculer_intervalles_occupation tous_vols;
  Parking.tri_heure_debut tous_vols hashtbl_parkings;
  Printf.printf "Affichage des informations des vols par parking:\n";
  Traitement_donnees.ecrire_statistiques_par_heure_csv tous_vols hashtbl_parkings "statistiques_par_heure.csv";


(* Afficher le nombre total d'avions dans la liste tous_vols *)
Printf.printf "Nombre total de vols : %d\n" (List.length vols);
Printf.printf "Nombre de départs 26R : %d\n" (List.length vols_pistes_26R);
Printf.printf "Nombre de départs 27L : %d\n" (List.length vols_pistes_27L);
(* Optimisation pour les deux pistes *)
Printf.printf "\nDébut de l'optimisation...\n";
let resultats_26R = optimiser_piste tous_vols "26R" in
let resultats_27L = optimiser_piste tous_vols "27L" in

(* Affichage des résultats *)
afficher_resultats_piste "26R" resultats_26R;
afficher_resultats_piste "27L" resultats_27L