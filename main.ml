module Vol = Vol
module Etot = Etot
module Extraire = Extraire
module Ttot = Ttot
module Dman = Dman
module Parking = Parking

(* Fonction principale *)
let () =
  let fichier = open_in "data/lfpg_flights.txt" in
  let vols = Vol.extraire_info fichier in
  close_in fichier;
  let vols_avec_etot = Etot.calculer_etot vols in
  let vols_pistes_26R = Etot.dep_qfu vols_avec_etot "26R" in
  let vols_pistes_27L = Etot.dep_qfu vols_avec_etot "267L" in
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
  let tous_vols = heure_parking_26L @ heure_parking_26R @ heure_parking_27L @ heure_parking_27R in
  let nombre_parkings_differents = Parking.nombre_parkings_differents vols in
  let liste_parking_differents = Parking.liste_parking_differents vols in
  let hashtbl_parkings = Parking.create_hashtbl_vide nombre_parkings_differents in
  Parking.remplir_hashtbl tous_vols liste_parking_differents hashtbl_parkings;
  Parking.tri_heure_debut tous_vols hashtbl_parkings;
  Vol.afficher_info tous_vols;
  Parking.afficher_hashtbl hashtbl_parkings;
  Ttot.retard_moyen_par_heure heure_parking_26R;
  Ttot.ecrire_retard_par_heure_csv heure_parking_27L "fic_15_1_26R.csv"