(** Module pour le calcul des temps estimés de décollage (ETOT) *)

(** Calcule l'ETOT pour une liste de vols
    @param vols Liste des vols à traiter
    @return Liste des vols avec leur ETOT calculé *)
val calculer_etot : Vol.t list -> Vol.t list

(** Affiche les ETOT des vols
    @param vols Liste des vols dont on veut afficher les ETOT *)
val afficher_info_etot : Vol.t list -> unit

(** Filtre les départs pour une piste donnée
    @param vols Liste des vols à filtrer
    @param piste Identifiant de la piste (ex: "26R")
    @return Liste des vols de départ pour cette piste *)
val dep_qfu : Vol.t list -> string -> Vol.t list

(** Filtre les arrivées pour une piste donnée
    @param vols Liste des vols à filtrer
    @param piste Identifiant de la piste (ex: "26L")
    @return Liste des vols d'arrivée pour cette piste *)
val arr_qfu : Vol.t list -> string -> Vol.t list

(** Trie les vols par ETOT croissant
    @param vols Liste des vols à trier
    @return Liste des vols triée par ETOT *)
val tri_etot : Vol.t list -> Vol.t list