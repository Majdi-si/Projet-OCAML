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
  let vols_piste_26L = Etot.arr_qfu vols_avec_etot "26L" in
  let dmans = Dman.calculer_dman vols_piste_26L in
  Vol.afficher_info dmans;
  let heure_piste = Dman.calculer_heure_parking dmans in
  let nombre_parkings = Parking.nombre_parkings_differents vols_piste_26L in
  Printf.printf "Nombre de parkings différents : %d\n" nombre_parkings;
  let liste_parkings = Parking.liste_parking_differents vols_piste_26L in
  let hash_table = Parking.ParkingHashtbl.create nombre_parkings in
  Parking.remplir_hashtbl vols_piste_26L liste_parkings hash_table;
  Parking.afficher_hashtbl hash_table;