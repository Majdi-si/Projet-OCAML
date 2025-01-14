(** Module pour la génération des statistiques *)

type config_csv = {
  nom_fichier : string;
  avec_optimisation : bool;
  afficher_conflits : bool
}

(** Calcule le nombre de créneaux ratés par heure
    @param vols Liste des vols à analyser
    @return Tableau des créneaux ratés par heure *)
val nb_creneaux_rates_par_heure : Vol.t list -> int array

(** Écrit les statistiques par heure dans un fichier CSV
    @param vols Liste des vols à analyser
    @param hashtbl_parkings Table de hachage des parkings
    @param config Configuration pour l'écriture du fichier CSV *)
val ecrire_statistiques_par_heure_csv : Vol.t list -> Vol.t list Parking.ParkingHashtbl.t -> config_csv -> unit