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
  (*Etot.afficher_info_etot vols_avec_etot;*)
  let vols_avec_ttot = Ttot.calculer_ttot vols_avec_etot in
  (*Ttot.afficher_info_ttot vols_avec_ttot;*)
  Vol.afficher_info vols_avec_ttot;
  Ttot.calcul_retard vols_avec_ttot;
  (*let tableau_ref = ref [||] in
  (* Réouvrir le fichier pour extraire les informations dans tableau_ref *)
  let fichier = Vol.ouvrir_fichier "data/lfpg_flights.txt" in
  let _ = Extraire.extraire_info fichier tableau_ref in
  Vol.fermer_fichier fichier*)