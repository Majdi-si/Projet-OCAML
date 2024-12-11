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
  let vols_piste_26R = Etot.dep_qfu vols_avec_etot "26R" in
  let vols_piste_27L = Etot.dep_qfu vols_avec_etot "27L" in
  let vols_tries_26R = Etot.tri_etot vols_piste_26R in
  let vols_tries_27L = Etot.tri_etot vols_piste_27L in
  let ttot_26R = Ttot.calculer_ttot vols_tries_26R in
  Vol.afficher_info ttot_26R;
  Ttot.calcul_retard ttot_26R;
