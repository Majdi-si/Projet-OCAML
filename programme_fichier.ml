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
    let info = Str.split (Str.regexp " ") ligne in
    match info with
    | type_vol :: indicatif :: turbulence :: parking :: qfu :: heure_debut :: heure_piste :: creneau :: points ->
        (type_vol, indicatif, turbulence, parking, qfu, heure_debut, heure_piste, creneau, points) :: (extraire_info fichier)
    | _ -> extraire_info fichier
  with
    End_of_file -> []

(*Programme pour fermer le fichier*)

let fermer_fichier fichier =
  close_in fichier

(*Programme pour afficher les informations*)

let afficher_info info =
  List.iter (fun (type_vol, indicatif, turbulence, parking, qfu, heure_debut, heure_piste, creneau, points) ->
    Printf.printf "%s %s %s %s %s %s %s %s\n" type_vol indicatif turbulence parking qfu heure_debut heure_piste creneau;
    List.iter (fun point -> Printf.printf "%s " point) points;
    Printf.printf "\n"
  ) info

(*Programme principal*)

let () =
  let fichier = ouvrir_fichier "data/lfpg_flights.txt" in
  let info = extraire_info fichier in
  fermer_fichier fichier;
  (*afficher_info info*)