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
        (* Recalculer tous les intervalles et conflits *)
        List.iter (fun vol ->
          vol.Vol.occupation_parking <- { lower = 0; upper = 0 }
        ) tous_vols;
        Parking.calculer_intervalles_occupation tous_vols;
        Parking.recalculer_conflits tous_vols hashtbl_parkings;
        menu_principal tous_vols hashtbl_parkings  (* Ajout de cette ligne *)
      
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
      
  | 7 -> (* Statistiques par piste *)
      Printf.printf "\n=== Statistiques par piste ===\n";
      let (p26r, p27l, p26l, p27r) = List.fold_left (fun (a,b,c,d) vol ->
        match vol.Vol.qfu with
        | "26R" -> (a+1,b,c,d)
        | "27L" -> (a,b+1,c,d)
        | "26L" -> (a,b,c+1,d)
        | "27R" -> (a,b,c,d+1)
        | _ -> (a,b,c,d)
      ) (0,0,0,0) tous_vols in
      Printf.printf "26R (Départs) : %d vols\n" p26r;
      Printf.printf "27L (Départs) : %d vols\n" p27l;
      Printf.printf "26L (Arrivées) : %d vols\n" p26l;
      Printf.printf "27R (Arrivées) : %d vols\n" p27r;
      menu_principal tous_vols hashtbl_parkings

  | 8 -> (* Analyser pics d'activité *)
      Printf.printf "\n=== Pics d'activité ===\n";
      let heures = Array.make 24 0 in
      List.iter (fun vol ->
        let heure = 
          if vol.Vol.type_vol = "DEP" then 
            vol.Vol.etot / 3600
          else 
            int_of_string vol.Vol.heure_debut / 3600 in
        if heure >= 0 && heure < 24 then
          heures.(heure) <- heures.(heure) + 1
      ) tous_vols;
      let max_vols = ref 0 in
      let heure_pic = ref 0 in
      Array.iteri (fun h nb ->
        if nb > !max_vols then (
          max_vols := nb;
          heure_pic := h
        );
        Printf.printf "Heure %02d: %d vols\n" h nb
      ) heures;
      Printf.printf "\nPic d'activité à %02dh avec %d vols\n" !heure_pic !max_vols;
      menu_principal tous_vols hashtbl_parkings

  | 9 -> (* Statistiques par catégorie *)
      Printf.printf "\n=== Statistiques par catégorie ===\n";
      let cats = Hashtbl.create 3 in
      List.iter (fun vol ->
        let cat = vol.Vol.turbulence in
        let count = try Hashtbl.find cats cat with Not_found -> 0 in
        Hashtbl.replace cats cat (count + 1)
      ) tous_vols;
      Hashtbl.iter (fun cat count ->
        Printf.printf "Catégorie %s: %d vols (%.1f%%)\n" 
          cat count (100. *. float_of_int count /. float_of_int (List.length tous_vols))
      ) cats;
      menu_principal tous_vols hashtbl_parkings 
    
  | 10 -> (* Quitter *)
      Printf.printf "Au revoir!\n"

  | _ -> (* Choix invalide *)
      Printf.printf "Choix invalide!\n";
      menu_principal tous_vols hashtbl_parkings


  


let () =
  let fichier_initial = "data/lfpg_flights.txt" in
  Printf.printf "Chargement du fichier initial : %s\n" fichier_initial;
  let fichier = open_in fichier_initial in
  let vols = Vol.extraire_info fichier in
  close_in fichier;


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
  let hashtbl_parkings = Parking.create_hashtbl_vide nombre_parkings_differents in
  Parking.remplir_hashtbl heure_parking_26L hashtbl_parkings;
  Parking.remplir_hashtbl heure_parking_26R hashtbl_parkings;
  Parking.remplir_hashtbl heure_parking_27L hashtbl_parkings;
  Parking.remplir_hashtbl heure_parking_27R hashtbl_parkings;
  let tous_vols = heure_parking_26L @ heure_parking_26R @ heure_parking_27L @ heure_parking_27R in
  Parking.calculer_intervalles_occupation tous_vols;
  Parking.tri_heure_debut tous_vols hashtbl_parkings;
  menu_principal tous_vols hashtbl_parkings