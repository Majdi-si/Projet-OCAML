module Vol = Vol
module Etot = Etot
module Extraire = Extraire
module Ttot = Ttot

(* Fonction principale *)
let () =
  let fichier = Vol.ouvrir_fichier "data/lfpg_flights.txt" in
  let vols = Vol.extraire_info fichier in
  Vol.fermer_fichier fichier;
  let info_etot = Etot.calculer_etot vols in
  let info_ttot = Ttot.calculer_ttot info_etot in
  (*Ttot.afficher_info_ttot info_ttot;*)
  Vol.afficher_info vols;
  Ttot.calcul_retard info_ttot;
  (*let tableau_ref = ref [||] in
  (* Réouvrir le fichier pour extraire les informations dans tableau_ref *)
  let fichier = Vol.ouvrir_fichier "data/lfpg_flights.txt" in
  let _ = Extraire.extraire_info fichier tableau_ref in
  Vol.fermer_fichier fichier*)