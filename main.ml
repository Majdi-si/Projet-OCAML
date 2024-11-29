module Vol = Vol
module Etot = Etot
module Extraire = Extraire

(* Fonction principale *)
let () =
  let fichier = Vol.ouvrir_fichier "data/lfpg_flights.txt" in
  let vols = Vol.extraire_info fichier in
  Vol.afficher_info vols;
  Vol.fermer_fichier fichier;
  (*let info_etot = Etot.calculer_etot vols in
  Etot.afficher_info_etot info_etot;*)
  (*let tableau_ref = ref [||] in
  (* Réouvrir le fichier pour extraire les informations dans tableau_ref *)
  let fichier = Vol.ouvrir_fichier "data/lfpg_flights.txt" in
  let _ = Extraire.extraire_info fichier tableau_ref in
  Vol.fermer_fichier fichier*)