type type_vol = DEP | ARR
type turbulence = H | M | L

type t = {
  type_vol : type_vol;
  indicatif : string;
  turbulence : turbulence;
  parking : string;
  qfu : string;
  heure_debut : int;
  heure_piste : int;
  creneau : int;
  points : string list;
  mutable etot : int;
  mutable ttot : int;
  (*mutable dman : int
  mutable heure_parking : int*)
}

val ouvrir_fichier : string -> in_channel
val drop : int -> 'a list -> 'a list
val extraire_info : in_channel -> t list
val fermer_fichier : in_channel -> unit
val afficher_info : t list -> unit
