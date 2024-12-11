module Vol = Vol
module Etot = Etot
module Extraire = Extraire
module Ttot = Ttot

(* Fonction principale *)
let () =
  let fichier = open_in "data/lfpg_flights.txt" in
  let vols = Vol.extraire_info fichier in
  close_in fichier;
  let vols_avec_etot = Etot.calculer_etot vols in
  let tri_etot = Etot.tri_etot vols_avec_etot in
  let vols_avec_ttot = Ttot.calculer_ttot tri_etot in
  Ttot.calcul_retard vols_avec_ttot;