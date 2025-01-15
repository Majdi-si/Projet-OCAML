module Params = struct
  type config = {
    parking_time : int;
    type_separation : int * int * int;
  }

  type config_csv = {
    nom_fichier : string;
    avec_optimisation : bool;
    afficher_conflits : bool
  }

  let current_config = ref {
    parking_time = 0;  (* Sera initialisé par l'utilisateur *)
    type_separation = (0, 0, 0)
  }

let init_config park_time sep1 sep2 sep3 =
  current_config := {
    parking_time = park_time;
    type_separation = (sep1, sep2, sep3)
  }

  let create_config parking_time type_separation = { 
    parking_time; 
    type_separation 
  }

  let create_config_csv nom_fichier avec_optimisation afficher_conflits = {
    nom_fichier;
    avec_optimisation;
    afficher_conflits
  }

  let set_config config =
    current_config := config

  let get_parking_time () = 
    !current_config.parking_time

  let get_separation_times () =
    !current_config.type_separation

  let get_choix () =
    print_string "\n=== Menu Principal ===\n";
    print_string "1. Générer statistiques\n";
    print_string "2. Optimiser séquence\n";
    print_string "3. Analyser conflits\n";
    print_string "4. Modifier paramètres\n";
    print_string "5. Afficher paramètres\n";
    print_string "6. Changer fichier de données\n";
    print_string "7. Statistiques par piste\n";  
    print_string "8. Analyser pics d'activité\n"; 
    print_string "9. Statistiques par catégorie\n"; 
    print_string "10. Quitter\n";
    print_string "Votre choix : ";
    read_int ()

  let get_nom_fichier_donnees () =
    print_string "Entrez le chemin du fichier de données : ";
    read_line ()

  let get_nom_fichier () =
    print_string "Nom du fichier CSV (sans extension) : ";
    let nom_base = read_line () in
    Printf.sprintf "%s.csv" nom_base

  let avec_optimisation () =
    print_string "Inclure optimisation (o/n) ? ";
    read_line () = "o"

  let afficher_conflits () =
    print_string "Afficher les conflits de parking (o/n) ? ";
    read_line () = "o"

  let get_piste () =
    print_string "Piste (26R/27L) : ";
    read_line ()

  let get_heure () =
    print_string "Heure (0-23) : ";
    read_int ()

  let modifier_parametres () =
    print_string "\n=== Modification des paramètres ===\n";
    print_string "Nouveau temps d'occupation parking (min) : ";
    let new_park_time = read_int () * 60 in    (* Conversion minutes -> secondes *)
    print_string "Nouveau temps séparation ML/HL/HM (min) : ";
    let new_sep_1 = read_int () * 60 in
    print_string "Nouveau temps séparation HH (min) : ";
    let new_sep_2 = read_int () * 60 in
    print_string "Nouveau temps séparation reste (min) : ";
    let new_sep_3 = read_int () * 60 in
    let new_config = create_config new_park_time (new_sep_1, new_sep_2, new_sep_3) in
    set_config new_config

  let afficher_parametres () =
    let curr_config = !current_config in
    Printf.printf "\n=== Paramètres actuels ===\n";
    Printf.printf "Temps occupation parking : %d minutes\n" (curr_config.parking_time / 60);
    let (s1, s2, s3) = curr_config.type_separation in
    Printf.printf "Temps séparation ML/HL/HM : %d minutes\n" (s1 / 60);
    Printf.printf "Temps séparation HH : %d minutes\n" (s2 / 60);
    Printf.printf "Temps séparation reste : %d minutes\n" (s3 / 60)
end



(* Programme principal avec menu *)
let () =
  (* Configuration initiale *)
  print_string "=== Configuration initiale ===\n";
  print_string "Entrez le temps d'occupation d'un parking en minutes (int) : ";
  let park_time = read_int () * 60 in

  print_string "Entrez le temps de separation ('M', 'L') | ('H', 'L') | ('H', 'M') en minutes: ";
  let sep_1 = read_int () * 60 in
  print_string "Entrez le temps de separation ('H', 'H') en minutes: ";
  let sep_2 = read_int () * 60 in
  print_string "Entrez le temps de separation pour le reste en minutes: ";
  let sep_3 = read_int () * 60 in

  let config = Params.create_config park_time (sep_1, sep_2, sep_3) in
  Params.set_config config;

  

