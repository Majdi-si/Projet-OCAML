(** Module pour la gestion des paramètres *)

module Params : sig
  type config = {
    parking_time : int;
    type_separation : int * int * int;
  }

  type config_csv = {
    nom_fichier : string;
    avec_optimisation : bool;
    afficher_conflits : bool
  }

  (** Crée une nouvelle configuration
      @param parking_time Temps d'occupation du parking (secondes)
      @param type_separation Temps de séparation (H/M/L) (secondes)
      @return Nouvelle configuration *)
  val create_config : int -> (int * int * int) -> config

  (** Crée une nouvelle configuration pour CSV
      @param nom_fichier Nom du fichier CSV
      @param avec_optimisation Indique si l'optimisation est activée
      @param afficher_conflits Indique si l'affichage des conflits est activé
      @return Nouvelle configuration CSV *)
  val create_config_csv : string -> bool -> bool -> config_csv

  (** Définit la configuration actuelle *)
  val set_config : config -> unit

  (** Obtient le temps d'occupation du parking (secondes) *)
  val get_parking_time : unit -> int

  (** Obtient les temps de séparation (secondes) *)
  val get_separation_times : unit -> (int * int * int)

  (** Affiche les paramètres actuels *)
  val afficher_parametres : unit -> unit

  (** Modifie les paramètres (parking_time, séparations...) *)
  val modifier_parametres : unit -> unit

  (** Obtient le choix du menu principal (int) *)
  val get_choix : unit -> int

  (** Nom du fichier de données *)
  val get_nom_fichier_donnees : unit -> string

  (** Nom du fichier CSV final *)
  val get_nom_fichier : unit -> string

  (** Indique si l'optimisation doit être incluse *)
  val avec_optimisation : unit -> bool

  (** Indique si les conflits de parking doivent être affichés *)
  val afficher_conflits : unit -> bool

  (** Retourne la piste choisie (string) *)
  val get_piste : unit -> string

  (** Retourne l'heure choisie (int) *)
  val get_heure : unit -> int
end