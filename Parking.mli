(** Module pour la gestion des parkings et des conflits *)

module ParkingHashtbl : Hashtbl.S with type key = string

(** Compte le nombre de parkings différents utilisés
    @param vols Liste des vols à analyser
    @return Nombre de parkings différents *)
val nombre_parkings_differents : Vol.t list -> int

(** Liste les parkings différents utilisés
    @param vols Liste des vols à analyser
    @return Liste des identifiants de parking *)
val liste_parking_differents : Vol.t list -> string list

(** Crée une table de hachage vide pour les parkings
    @param nombre_parkings Taille initiale de la table
    @return Table de hachage vide *)
val create_hashtbl_vide : int -> Vol.t list ParkingHashtbl.t

(** Remplit la table de hachage avec les vols
    @param vols Liste des vols à ajouter
    @param ht Table de hachage à remplir *)
val remplir_hashtbl : Vol.t list -> Vol.t list ParkingHashtbl.t -> unit

(** Calcule les intervalles d'occupation des parkings
    @param vols Liste des vols à traiter *)
val calculer_intervalles_occupation : Vol.t list -> unit

(** Trie les vols par heure de début
    @param vols Liste des vols à trier
    @param hashtbl Table de hachage des parkings *)
val tri_heure_debut : Vol.t list -> Vol.t list ParkingHashtbl.t -> unit

(** Analyse et affiche les conflits de parking pour chaque parking
    @param vols Liste des vols à analyser
    @param ht Table de hachage des parkings *)
val calcul_conflit_parking : Vol.t list -> Vol.t list ParkingHashtbl.t -> unit

(** Recalcule les conflits de parking après modification des paramètres
    @param vols Liste des vols à analyser
    @param ht Table de hachage *)
val recalculer_conflits : Vol.t list -> Vol.t list ParkingHashtbl.t -> unit
