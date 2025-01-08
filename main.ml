module Vol = Vol
module Etot = Etot
module Extraire = Extraire
module Ttot = Ttot
module Dman = Dman
module Parking = Parking
module Traitement_donnees = Traitement_donnees
module Optimisation = Optimisation
module Params = Parametre.Params


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
  (* Demander l'heure à optimiser *)
  Printf.printf "Entrez l'heure à optimiser (0-23): ";
  let heure_a_optimiser = read_int () in
  
  Printf.printf "Choisissez une pistes de départ à optimiser (26R, 27L): ";
  let piste_a_optimiser = read_line () in

  
  (* Filtrer les vols de départ de l'heure choisie *)
  let vols_heure = List.filter (fun v -> 
    let heure = v.Vol.etot / 3600 in
    heure = heure_a_optimiser && v.Vol.type_vol = "DEP" && v.Vol.qfu = piste_a_optimiser
  ) tous_vols in
  
  if List.length vols_heure = 0 then
    Printf.printf "Aucun vol de départ à optimiser pour l'heure %d\n" heure_a_optimiser
  else (
    Printf.printf "Optimisation des séquences de départ pour l'heure %d (%d départs)...\n" 
      heure_a_optimiser (List.length vols_heure);
    let sequence_optimale = Optimisation.optimiser_sequence vols_heure in
    Printf.printf "Affichage de la séquence optimale pour l'heure %d :\n" heure_a_optimiser;
    Optimisation.afficher_sequence sequence_optimale;
    
    Printf.printf "\nStatistiques des départs pour l'heure %d:\n" heure_a_optimiser;
    Printf.printf "Nombre de départs : %d\n" (List.length vols_heure);
    let retard_total = sequence_optimale.cout in
    Printf.printf "Retard total : %d secondes (%.2f minutes)\n" 
      retard_total (float_of_int retard_total /. 60.0);
    Printf.printf "Retard moyen : %.2f minutes\n" 
      (float_of_int retard_total /. float_of_int (List.length vols_heure) /. 60.0)
  )