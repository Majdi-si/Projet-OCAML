module Vol = Vol
module Etot = Etot
module Extraire = Extraire
module Ttot = Ttot
module Dman = Dman

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
  Vol.afficher_info heure_piste;
