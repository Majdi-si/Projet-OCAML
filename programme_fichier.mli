type vol = {
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
}

val ouvrir_fichier : string -> in_channel

val drop : int -> 'a list -> 'a list

val extraire_info : in_channel -> vol list

val fermer_fichier : in_channel -> unit

val afficher_info : vol list -> unit

val calculer_etot : vol list -> vol list

val afficher_info_etot : vol list -> unit