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


(*Programme pour extraire les informations*)


let rec extraire_info fichier =
  try
    let ligne = input_line fichier in
    let champs = String.split_on_char ',' ligne in
    match champs with
    | type_vol :: indicatif :: turbulence :: parking :: qfu :: heure_debut :: heure_piste :: creneau :: points :: _ ->
      let vol = {
        type_vol;
        indicatif;
        turbulence;
        parking;
        qfu;
        heure_debut;
        heure_piste;
        creneau;
        points = String.split_on_char ';' points;
        etot = 0;
      } in
      vol :: extraire_info fichier
    | _ -> failwith "Format de ligne invalide"
  with
    End_of_file -> []

(*Programme pour fermer le fichier*)

let fermer_fichier fichier =
  close_in fichier

(*Programme pour afficher les informations*)

let afficher_info vol =
  List.iter (fun vol ->
    Printf.printf "Type de vol : %s\nIndicatif : %s\nTurbulence : %s\nParking : %s\nQFU : %s\nHeure de début : %s\nHeure piste : %s\nCréneau : %s\nPoints : %s\n"
      vol.type_vol vol.indicatif vol.turbulence vol.parking vol.qfu vol.heure_debut vol.heure_piste vol.creneau (String.concat ";" vol.points);
  ) vol



(* Fonction pour calculer l'ETOT*)
let calculer_etot liste = 
  List.map(fun(type_vol, indicatif, turbulence, parking, qfu, heure_debut, heure_piste, creneau, points)-> 
  let etot = 
        if type_vol = "DEP" then
          let heure_debut_int = int_of_string heure_debut in
          let nombre_points = List.length points in
          heure_debut_int + (5 * nombre_points)
        else
        0
      in
      (type_vol, indicatif, turbulence, parking, qfu, heure_debut, heure_piste, creneau, points, etot)
      )liste

(* Fonction pour afficher la liste avec ETOT *)
let afficher_info_etot liste =
  List.iter (fun (type_vol, indicatif, turbulence, parking, qfu, heure_debut, heure_piste, creneau, points, etot) ->
    Printf.printf "Indicatif : %s -> ETOT: %d\n" indicatif etot;
    )liste

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
  (*let info_etot = calculer_etot info in
  afficher_info_etot info_etot ;*)
  (*let liste_piste = vol_par_piste info_etot in
  afficher_liste_piste liste_piste;*)

  afficher_info vol;
  ()

