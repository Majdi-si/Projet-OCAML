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

(*Programme pour ouverture et lecture du fichier*)


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
    let points = drop 8 champs in
    let etot = 0 in
    (* Créer un enregistrement avec les informations extraites *)
    {type_vol; indicatif; turbulence; parking; qfu; heure_debut; heure_piste; creneau; points; etot} :: extraire_info fichier
  with
    (* Gérer la fin du fichier *)
    End_of_file -> []


(*Programme pour fermer le fichier*)

let fermer_fichier fichier =
  close_in fichier

(* Fonction pour afficher les informations d'un vol *)
let afficher_info vols =
  List.iter (fun vol ->
    Printf.printf "Type de vol : %s\n" vol.type_vol;
    Printf.printf "Indicatif : %s\n" vol.indicatif;
    Printf.printf "Turbulence : %s\n" vol.turbulence;
    Printf.printf "Parking : %s\n" vol.parking;
    Printf.printf "QFU : %s\n" vol.qfu;
    Printf.printf "Heure de début : %s\n" vol.heure_debut;
    Printf.printf "Heure piste : %s\n" vol.heure_piste;
    Printf.printf "Créneau : %s\n" vol.creneau;
    Printf.printf "Points : %s\n" (String.concat ", " vol.points);
    Printf.printf "ETOT : %d\n" vol.etot;
    Printf.printf "\n";
  ) vols
(*Programme pour calculer l'ETOT*)



(* Fonction pour calculer l'ETOT*)
let calculer_etot vols = 
  List.map (fun vol -> 
    let etot = 
      if vol.type_vol = "DEP" then
        let heure_debut_int = int_of_string vol.heure_debut in
        let nombre_points = List.length vol.points in
        heure_debut_int + (5 * nombre_points)
      else
        0
    in
    vol.etot <- etot;
    vol
  ) vols

(* Fonction pour afficher les informations d'un vol avec l'ETOT *)

let afficher_info_etot vols =
  List.iter (fun vol ->
    Printf.printf "ETOT : %d\n" vol.etot;
    Printf.printf "\n";
  ) vols


(* Fonction pour regrouper les vols par piste 
let vol_par_piste liste = 
  (* Crée une liste de tuples (qfu, info) où info est un tuple contenant les informations du vol *)
  let liste_piste = List.map (fun (type_vol, indicatif, turbulence, parking, qfu, heure_debut, heure_piste, creneau, points, etot) -> 
    (qfu, (type_vol, indicatif, turbulence, parking, qfu, heure_debut, heure_piste, creneau, points, etot))
  ) liste in

  (* Trie la liste des vols par qfu *)
  let liste_piste_triee = List.sort (fun (qfu1, _) (qfu2, _) -> compare qfu1 qfu2) liste_piste in

  (* Agrège les vols par qfu *)
  let liste_piste_agregee = List.fold_left (fun acc (qfu, info) ->  (*fold_left prend une liste et un accumulateur et applique une fonction à chaque élément de la liste*)
    match acc with
    | [] -> [(qfu, [info])]  (* Si la liste est vide, crée une nouvelle entrée *)
    | (qfu', liste) :: reste when qfu = qfu' -> (qfu', info :: liste) :: reste  (* Ajoute le vol à la liste existante pour ce qfu *)
    | _ -> (qfu, [info]) :: acc  (* Crée une nouvelle entrée pour un nouveau qfu *)
  ) [] liste_piste_triee in

  (* Retourne la liste agrégée *)
  liste_piste_agregee



let afficher_liste_piste liste = 
  List.iter (fun (qfu, liste) -> 
    Printf.printf "Piste %s\n" qfu;
    List.iter (fun (type_vol, indicatif, turbulence, parking, qfu, heure_debut, heure_piste, creneau, points, etot) -> 
      Printf.printf "Indicatif : %s -> ETOT: %d\n" indicatif etot;
    )liste
  )liste
*)


(*Programme principal*)

let () =
  let fichier = ouvrir_fichier "data/lfpg_flights.txt" in
  let vol = extraire_info fichier in
  fermer_fichier fichier;
  let info_etot = calculer_etot vol in
  afficher_info_etot info_etot;
  (*let liste_piste = vol_par_piste info_etot in
  afficher_liste_piste liste_piste;*)

  (*afficher_info vol;*)
  ()

