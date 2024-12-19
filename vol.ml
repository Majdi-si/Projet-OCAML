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
    (* Séparer la ligne en champs en utilisant l'espace comme séparateur *)
    let champs = String.split_on_char ' ' ligne in
    (* Extraire chaque champ en utilisant les indices appropriés *)
    let type_vol = match List.nth champs 0 with
      | "DEP" -> DEP
      | "ARR" -> ARR
      | _ -> failwith "Invalid type_vol" in
    let indicatif = List.nth champs 1 in
    let turbulence = match List.nth champs 2 with
      | "H" -> H
      | "M" -> M
      | "L" -> L
      | _ -> failwith "Invalid turbulence" in
    let parking = List.nth champs 3 in
    let qfu = List.nth champs 4 in
    let heure_debut = int_of_string (List.nth champs 5) in
    let heure_piste = int_of_string (List.nth champs 6) in
    let creneau = int_of_string (List.nth champs 7) in
    let points = drop 8 champs in      (* Récupérer les points à partir de l'indice 8 *)
    let etot = 0 in
    let ttot = 0 in
    (* Créer un enregistrement avec les informations extraites *)
    {type_vol; indicatif; turbulence; parking; qfu; heure_debut; heure_piste; creneau; points; etot; ttot} :: extraire_info fichier
  with
    (* Gérer la fin du fichier *)
    End_of_file -> []

(*Programme pour fermer le fichier*)
let fermer_fichier fichier =
  close_in fichier

(* Fonction pour afficher les informations d'un vol *)
let rec afficher_info = function
  | [] -> ()
  | {type_vol; indicatif; turbulence; parking; qfu; heure_debut; heure_piste; creneau; points; etot; ttot} :: vols ->
    Printf.printf "Type de vol : %s\n" (match type_vol with DEP -> "DEP" | ARR -> "ARR");
    Printf.printf "Indicatif : %s\n" indicatif;
    Printf.printf "Turbulence : %s\n" (match turbulence with H -> "H" | M -> "M" | L -> "L");
    Printf.printf "Parking : %s\n" parking;
    Printf.printf "QFU : %s\n" qfu;
    Printf.printf "Heure de début : %d\n" heure_debut;
    Printf.printf "Heure piste : %d\n" heure_piste;
    Printf.printf "Créneau : %d\n" creneau;
    Printf.printf "Points : %s\n" (String.concat ", " points);
    Printf.printf "ETOT : %d\n" etot;
    Printf.printf "TTOT : %d\n" ttot;
    Printf.printf "\n";
    afficher_info vols
