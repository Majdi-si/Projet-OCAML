module Params = struct
  type config = {
    parking_time : int;
    type_separation : int * int * int;
  }

  (* Variable mutable pour stocker la config courante *)
  let current_config = ref {
    parking_time = 15 * 60; 
    type_separation = (180, 120, 60)
  }

  (* Fonction pour créer une configuration *)
  let create_config parking_time type_separation = { 
    parking_time; 
    type_separation 
  }

  (* Fonction pour mettre à jour la config *)
  let set_config config =
    current_config := config

  (* Fonctions pour accéder aux paramètres *)
  let get_parking_time () = 
    !current_config.parking_time

  let get_separation_times () =
    !current_config.type_separation
end

(* Programme principal *)
let () =
  print_string "Entrez le temps d'occupation d'un parking en minutes (int) : ";
  let park_time = read_int () * 60 in

  print_string "Entrez le temps de separation ('M', 'L') | ('H', 'L') | ('H', 'M') en minutes: ";
  let sep_1 = read_int () * 60 in
  print_string "Entrez le temps de separation ('H', 'H') en minutes: ";
  let sep_2 = read_int () * 60 in
  print_string "Entrez le temps de separation pour le reste en minutes: ";
  let sep_3 = read_int () * 60 in

  (* Créer et sauvegarder la config *)
  let config = Params.create_config park_time (sep_1, sep_2, sep_3) in
  Params.set_config config