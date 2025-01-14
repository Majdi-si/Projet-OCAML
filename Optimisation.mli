(** Module pour l'optimisation des séquences de départ *)

type sequence = {
  vols : Vol.t list;
  cout : int
}

(** Optimise une séquence de vols pour minimiser les retards
    @param vols Liste des vols à optimiser
    @return Séquence optimisée *)
val optimiser_sequence : Vol.t list -> sequence

(** Optimise les vols jusqu'à une heure donnée
    @param vols Liste des vols à optimiser
    @param heure_cible Heure jusqu'à laquelle optimiser
    @param piste Identifiant de la piste
    @return Option de séquence optimisée *)
val optimiser_jusqu_a_heure : Vol.t list -> int -> string -> sequence option

(** Affiche les résultats d'une séquence optimisée
    @param sequence Séquence à afficher *)
val afficher_sequence : sequence -> unit

(** Affiche les résultats détaillés d'une séquence optimisée optionnelle
    @param sequence Option de séquence optimisée *)
val afficher_resultats_sequence : sequence option -> unit