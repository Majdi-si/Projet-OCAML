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

(*Prgramme pour ouverture et lecture du fichier*)
let ouvrir_fichier nom_fichier =
  try
    open_in nom_fichier
  with
    Sys_error msg -> failwith msg


(* Fonction pour obtenir une sous-liste à partir d'un certain index *)
let rec drop n lst =
  match lst with
  | [] -> []
  | _::t -> if n = 0 then lst else drop (n-1) t

(* Fonction récursive pour extraire les informations d'un fichier *)
let rec extraire_info fichier =
  try
    (* Lire une ligne du fichier *)
    let ligne = input_line fichier in
    (* Séparer la ligne en champs en utilisant la virgule comme séparateur *)
    let champs = String.split_on_char ' ' ligne in
    (* Extraire chaque champ en utilisant les indices appropriés *)
    let type_vol = List.nth champs 0 in
    let indicatif = List.nth champs 1 in
    let turbulence = List.nth champs 2 in
    let parking = List.nth champs 3 in
    let qfu = List.nth champs 4 in
    let heure_debut = List.nth champs 5 in
    let heure_piste = List.nth champs 6 in
    let creneau = List.nth champs 7 in
    let points = drop 8 champs in      (* Récupérer les points à partir de l'indice 8 *)
    let etot = 0 in
    let ttot = 0 in
    let dman = 0 in
    let heure_parking = 0 in
    (* Créer un enregistrement avec les informations extraites *)
    {type_vol; indicatif; turbulence; parking; qfu; heure_debut; heure_piste; creneau; points; etot; ttot; dman; heure_parking; occupation_parking = {lower = 0; upper = 0}} :: extraire_info fichier
  with
    (* Gérer la fin du fichier *)
    End_of_file -> []


(*Programme pour fermer le fichier*)

let fermer_fichier fichier =
  close_in fichier

(* Fonction pour afficher les informations d'un vol *)
let rec afficher_info = function
  | [] -> ()
  | {type_vol; indicatif; turbulence; parking; qfu; heure_debut; heure_piste; creneau; points; etot; ttot; dman; heure_parking; occupation_parking = {lower; upper}} :: vols ->
    Printf.printf "Type de vol : %s\n" type_vol;
    Printf.printf "Indicatif : %s\n" indicatif;
    Printf.printf "Turbulence : %s\n" turbulence;
    Printf.printf "Parking : %s\n" parking;
    Printf.printf "QFU : %s\n" qfu;
    Printf.printf "Heure de début : %s\n" heure_debut;
    Printf.printf "Heure piste : %s\n" heure_piste;
    Printf.printf "Créneau : %s\n" creneau;
    Printf.printf "Points : %s\n" (String.concat ", " points);
    Printf.printf "ETOT : %d\n" etot;
    Printf.printf "TTOT : %d\n" ttot;
    Printf.printf "DMAN : %d\n" dman;
    Printf.printf "Heure parking : %d\n" heure_parking;
    Printf.printf "Occupation parking : %d-%d\n" lower upper;
    Printf.printf "\n";
    afficher_info vols



 

    
