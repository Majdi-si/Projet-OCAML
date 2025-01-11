module Vol = Vol
module Etot = Etot
module Extraire = Extraire
module Ttot = Ttot
module Dman = Dman
module Parking = Parking
module Traitement_donnees = Traitement_donnees
module Optimisation = Optimisation
module Params = Parametre.Params

let rec menu_principal tous_vols hashtbl_parkings =
  match Params.get_choix () with
  | 1 -> (* Générer statistiques *)
      let config = {
        Traitement_donnees.nom_fichier = Params.get_nom_fichier();
        avec_optimisation = Params.avec_optimisation();
        afficher_conflits = true
      } in
      Traitement_donnees.ecrire_statistiques_par_heure_csv tous_vols hashtbl_parkings config;
      menu_principal tous_vols hashtbl_parkings
      
  | 2 -> (* Optimiser séquence *)
      let piste = Params.get_piste() in
      let heure = Params.get_heure() in
      let resultats = Optimisation.optimiser_jusqu_a_heure tous_vols heure piste in
      Optimisation.afficher_resultats_sequence resultats;
      menu_principal tous_vols hashtbl_parkings
      
  | 3 -> (* Analyser conflits *)
      Parking.calcul_conflit_parking tous_vols hashtbl_parkings;
      menu_principal tous_vols hashtbl_parkings
      
    | 4 -> (* Modifier paramètres *)
      Params.modifier_parametres();
      (* Recalculer les intervalles après modification des paramètres *)
      Parking.calculer_intervalles_occupation tous_vols;
      Parking.tri_heure_debut tous_vols hashtbl_parkings;
      menu_principal tous_vols hashtbl_parkings
      
  | 5 -> (* Afficher paramètres *)
      Params.afficher_parametres();
      menu_principal tous_vols hashtbl_parkings

  | 6 -> (* Changer fichier de données *)
    let nouveau_fichier = Params.get_nom_fichier_donnees() in
    Printf.printf "Chargement du fichier %s...\n" nouveau_fichier;
    let fichier = open_in nouveau_fichier in
    let nouveaux_vols = Vol.extraire_info fichier in
    close_in fichier;
    
    (* Refaire tous les calculs avec les nouvelles données *)
    let vols_avec_etot = Etot.calculer_etot nouveaux_vols in
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
    
    let nouveau_hashtbl_parkings = Parking.create_hashtbl_vide 
      (Parking.nombre_parkings_differents nouveaux_vols) in
    Parking.remplir_hashtbl heure_parking_26L nouveau_hashtbl_parkings;
    Parking.remplir_hashtbl heure_parking_26R nouveau_hashtbl_parkings;
    Parking.remplir_hashtbl heure_parking_27L nouveau_hashtbl_parkings;
    Parking.remplir_hashtbl heure_parking_27R nouveau_hashtbl_parkings;
    
    let nouveaux_tous_vols = heure_parking_26L @ heure_parking_26R @ 
                            heure_parking_27L @ heure_parking_27R in
    Parking.calculer_intervalles_occupation nouveaux_tous_vols;
    Parking.tri_heure_debut nouveaux_tous_vols nouveau_hashtbl_parkings;
    
    Printf.printf "Nouveau fichier chargé avec succès!\n";
    menu_principal nouveaux_tous_vols nouveau_hashtbl_parkings
      
  | 7 -> (* Quitter *)
      Printf.printf "Au revoir !\n"
      
  | _ ->
      Printf.printf "Choix invalide\n";
      menu_principal tous_vols hashtbl_parkings  


  


let () =
  let fichier_initial = "data/lfpg_flights.txt" in
  Printf.printf "Chargement du fichier initial : %s\n" fichier_initial;
  let fichier = open_in fichier_initial in
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
  let tous_vols = heure_parking_26L @ heure_parking_26R @ heure_parking_27L @ heure_parking_27R in
  Parking.calculer_intervalles_occupation tous_vols;
  Parking.tri_heure_debut tous_vols hashtbl_parkings;
  menu_principal tous_vols hashtbl_parkings