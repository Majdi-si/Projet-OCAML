(** Module pour le calcul des temps cibles de décollage (TTOT) *)

(** Calcule le temps de séparation entre deux catégories de turbulence
    @param cat1 Catégorie du premier avion ("H", "M" ou "L")
    @param cat2 Catégorie du deuxième avion
    @return Temps de séparation en secondes *)
val separation_time : string -> string -> int

(** Calcule les TTOT pour une liste de vols
    @param vols Liste des vols à traiter
    @return Liste des vols avec leur TTOT calculé *)
val calculer_ttot : Vol.t list -> Vol.t list

(** Affiche les TTOT des vols
    @param vols Liste des vols dont on veut afficher les TTOT *)
val afficher_info_ttot : Vol.t list -> unit

(** Calcule le retard moyen par heure
    @param vols Liste des vols à analyser *)
val retard_moyen_par_heure : Vol.t list -> unit

(** Calcule le retard moyen global
    @param vols Liste des vols à analyser *)
val retard_moyen_global : Vol.t list -> unit