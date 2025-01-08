(* Module pour définir la structure des paramètres *)
module Params = struct
  (* Définition d'un type record pour stocker les paramètres *)
  type config = {
    parking_time : int;                 (* Premier paramètre : int *)
    type_separation : int * int * int; (* Deuxième paramètre : int * int * int *)
  }

  (* Fonction pour créer une configuration *)
  let create_config parking_time type_separation = { parking_time; type_separation }
end

(* Programme principal à mettre dans le main normalement *)
let () =
  (* Lire les paramètres depuis l'entrée utilisateur *)
  print_string "Entrez le temps d'occupation d'un parking (int) : ";
  let park_time = read_int () in

  print_string "Entrez le temps de separation ('M', 'L') | ('H', 'L') | ('H', 'M'): ";
  let sep_1 = read_int () in
  print_string "Entrez le temps de separation ('H', 'H'): ";
  let sep_2 = read_int () in
  print_string "Entrez le temps de separation pour le reste : ";
  let sep_3 = read_int () in

  (* Créer la structure de configuration *)
  let config = Params.create_config park_time (sep_1, sep_2, sep_3) in
  
  ()