(** Type représentant un intervalle de temps avec des bornes modifiables *)
type interval = { 
  mutable lower: int;  (** Borne inférieure *)
  mutable upper: int   (** Borne supérieure *)
}

(** Type représentant un vol avec ses caractéristiques *)
type t = {
  type_vol : string;        (** Type de vol : "DEP" pour départ, "ARR" pour arrivée *)
  indicatif : string;       (** Identifiant unique du vol *)
  turbulence : string;      (** Catégorie de turbulence : "L", "M" ou "H" *)
  parking : string;         (** Identifiant du parking assigné *)
  qfu : string;            (** Orientation de la piste utilisée *)
  heure_debut : string;     (** Heure de début en secondes depuis minuit *)
  heure_piste : string;     (** Heure prévue sur la piste *)
  creneau : string;        (** Créneau NMOC assigné ou "_" si pas de créneau *)
  points : string list;    (** Liste des points de la trajectoire *)
  mutable etot : int;      (** Estimated Take-Off Time (temps estimé de décollage) *)
  mutable ttot : int;      (** Target Take-Off Time (temps cible de décollage) *)
  mutable dman : int;      (** Departure Manager Time (temps de départ du parking) *)
  mutable heure_parking : int; (** Heure d'arrivée au parking *)
  mutable occupation_parking : interval  (** Intervalle d'occupation du parking *)
}

(** Ouvre un fichier en lecture
    @param nom_fichier Chemin du fichier à ouvrir
    @return Le descripteur de fichier ouvert
    @raise Sys_error si le fichier ne peut pas être ouvert *)
val ouvrir_fichier : string -> in_channel

(** Supprime les n premiers éléments d'une liste
    @param n Nombre d'éléments à supprimer
    @param lst Liste source
    @return La liste privée des n premiers éléments *)
val drop : int -> 'a list -> 'a list

(** Extrait les informations des vols depuis un fichier
    @param fichier Descripteur du fichier à lire
    @return La liste des vols extraits du fichier *)
val extraire_info : in_channel -> t list

(** Ferme un fichier précédemment ouvert
    @param fichier Descripteur du fichier à fermer *)
val fermer_fichier : in_channel -> unit

(** Affiche les informations détaillées de tous les vols
    @param vols Liste des vols à afficher *)
val afficher_info : t list -> unit