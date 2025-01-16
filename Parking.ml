module Vol = Vol
module Dman = Dman
module Params = Parametre.Params

module ParkingHashtbl = Hashtbl.Make(struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end)

let nombre_parkings_differents (vols : Vol.t list) : int =
  let rec aux acc = function
    | [] -> acc
    | vol :: reste ->
      if List.mem vol.Vol.parking acc then
        aux acc reste
      else
        aux (vol.Vol.parking :: acc) reste
  in
  List.length (aux [] vols)

let liste_parking_differents (vols : Vol.t list) : string list =
  let rec aux acc = function
    | [] -> acc
    | vol :: reste ->
      if List.mem vol.Vol.parking acc then
        aux acc reste
      else
        aux (vol.Vol.parking :: acc) reste
  in
  aux [] vols

let afficher_liste_parkings (liste_parkings : string list) : unit =
  List.iter (fun parking -> Printf.printf "Parking: %s\n" parking) liste_parkings

let create_hashtbl_vide (nombre_parkings : int) : Vol.t list ParkingHashtbl.t =
  ParkingHashtbl.create nombre_parkings


let remplir_hashtbl (vols : Vol.t list) (ht : Vol.t list ParkingHashtbl.t) : unit =
  List.iter (fun vol ->
    let parking = vol.Vol.parking in
    if ParkingHashtbl.mem ht parking then 
      let vols_pour_parking = ParkingHashtbl.find ht parking in
      ParkingHashtbl.replace ht parking (vol :: vols_pour_parking)
    else
      ParkingHashtbl.add ht parking [vol]
  ) vols


let afficher_hashtbl (ht : Vol.t list ParkingHashtbl.t) : unit =
  let total_vols = ref 0 in
  ParkingHashtbl.iter (fun parking vols ->
    total_vols := !total_vols + List.length vols;
    Printf.printf "Parking: %s\n" parking;
    Printf.printf "Nombre de vols pour ce parking: %d\n" (List.length vols);
    List.iter (fun vol -> Printf.printf "  %s\n" vol.Vol.indicatif) vols
  ) ht;
  Printf.printf "Nombre total de vols: %d\n" !total_vols

let info_vol_par_parking (vols : Vol.t list) (ht : Vol.t list ParkingHashtbl.t) : unit =
  ParkingHashtbl.iter (fun parking vols ->
    Printf.printf "Parking: %s\n" parking;
    List.iter (fun vol ->
      Printf.printf "  Indicatif: %s\n" vol.Vol.indicatif;
      Printf.printf "  Type de vol: %s\n" vol.Vol.type_vol;
      Printf.printf "  Heure de début: %s\n" vol.Vol.heure_debut;
      Printf.printf "  Heure de parking: %d\n" vol.Vol.heure_parking;
      Printf.printf "  Occupation parking: [%d, %d]\n" vol.Vol.occupation_parking.lower vol.Vol.occupation_parking.upper
    ) vols
  ) ht


let tri_heure_debut (vols : Vol.t list) (ht : Vol.t list ParkingHashtbl.t) : unit =
  ParkingHashtbl.iter (fun parking vols ->
    let vols_tries = List.sort (fun vol1 vol2 ->
      compare vol1.Vol.occupation_parking.lower vol2.Vol.occupation_parking.lower
    ) vols in
    ParkingHashtbl.replace ht parking vols_tries
  ) ht

let calculer_intervalles_occupation (vols : Vol.t list) : unit =
  let parking_time = Params.get_parking_time () in 
  Printf.printf "Temps d'occupation parking: %d\n" parking_time;
  List.iter (fun (vol : Vol.t) ->
    match vol.type_vol with
    | "ARR" -> 
        let heure_parking = vol.heure_parking in
        vol.occupation_parking <- { 
          lower = heure_parking;
          upper = heure_parking + parking_time 
        }
    | "DEP" -> 
        let heure_dman = vol.dman in
        vol.occupation_parking <- { 
          lower = heure_dman - parking_time;
          upper = heure_dman 
        }
    | _ -> ()
  ) vols

let afficher_intervalles_occupation (vols : Vol.t list) : unit =
  List.iter (fun (vol : Vol.t) ->
    Printf.printf "Indicatif: %s\n" vol.indicatif;
    Printf.printf "Occupation parking: [%d, %d]\n" vol.occupation_parking.lower vol.occupation_parking.upper
  ) vols
  let get_vol_by_indicatif (vols : Vol.t list) (indicatif : string) : Vol.t option =
    List.find_opt (fun vol -> vol.Vol.indicatif = indicatif) vols

let calcul_conflit_parking (vols : Vol.t list) (ht : Vol.t list ParkingHashtbl.t) : unit =
  ParkingHashtbl.iter (fun parking vols ->
    let rec count_conflicts acc = function
      | [] -> acc
      | vol1 :: rest ->
        let conflicts = List.fold_left (fun count vol2 ->
          if vol1 != vol2 &&
             vol1.Vol.occupation_parking.lower < vol2.Vol.occupation_parking.upper &&
             vol1.Vol.occupation_parking.upper > vol2.Vol.occupation_parking.lower
          then count + 1
          else count
        ) 0 rest in
        count_conflicts (acc + conflicts) rest
    in
    let total_conflicts = count_conflicts 0 vols in
    Printf.printf "Parking: %s, Conflits: %d\n" parking total_conflicts
  ) ht

let count_conflicts (vols : Vol.t list) : int =
  let total_conflicts = ref 0 in
  List.iter (fun vol1 ->
    let heure1 = 
      if vol1.Vol.type_vol = "DEP" then vol1.Vol.dman / 3600
      else vol1.Vol.heure_parking / 3600 
    in
    List.iter (fun vol2 ->
      if vol1 != vol2 && vol1.Vol.parking = vol2.Vol.parking then
        let heure2 = 
          if vol2.Vol.type_vol = "DEP" then vol2.Vol.dman / 3600
          else vol2.Vol.heure_parking / 3600
        in
        (* Vérifier que les conflits sont dans la même heure *)
        if heure1 = heure2 && 
           vol1.Vol.occupation_parking.lower < vol2.Vol.occupation_parking.upper &&
           vol1.Vol.occupation_parking.upper > vol2.Vol.occupation_parking.lower then
          incr total_conflicts
    ) vols
  ) vols;
  !total_conflicts / 2

let recalculer_conflits vols hashtbl =
  calculer_intervalles_occupation vols;
  tri_heure_debut vols hashtbl;

  