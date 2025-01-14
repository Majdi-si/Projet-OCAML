(** Module pour le calcul des temps de départ du parking (DMAN) *)

(** Calcule le DMAN pour une liste de vols
    @param vols Liste des vols à traiter
    @return Liste des vols avec leur DMAN calculé *)
val calculer_dman : Vol.t list -> Vol.t list

(** Calcule l'heure d'arrivée au parking pour une liste de vols
    @param vols Liste des vols à traiter
    @return Liste des vols avec leur heure d'arrivée au parking calculée *)
val calculer_heure_parking : Vol.t list -> Vol.t list