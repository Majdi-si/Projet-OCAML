type interval = { mutable lower: int; mutable upper: int }

type t = {
  type_vol : string;
  indicatif : string;
  turbulence : string;
  parking : string;
  qfu : string;
  heure_debut : string;
  heure_piste : string;
  creneau : string;
  points : string list;
  mutable etot : int;
  mutable ttot : int;
  mutable dman : int;
  mutable heure_parking : int;
  mutable occupation_parking : interval;
}

val ouvrir_fichier : string -> in_channel
val drop : int -> 'a list -> 'a list
val extraire_info : in_channel -> t list
val fermer_fichier : in_channel -> unit
val afficher_info : t list -> unit
