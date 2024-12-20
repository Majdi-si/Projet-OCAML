module Vol = Vol
module Dman = Dman

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

let remplir_hashtbl (vols : Vol.t list) (liste_parkings : string list) (ht : Vol.t list ParkingHashtbl.t) : unit =
  List.iter (fun vol ->
    let parking = vol.Vol.parking in
    let vols_pour_parking = ParkingHashtbl.find ht parking in
    ParkingHashtbl.replace ht parking (vol :: vols_pour_parking)
  ) vols



let afficher_hashtbl (ht : Vol.t list ParkingHashtbl.t) : unit =
  ParkingHashtbl.iter (fun parking vols ->
    Printf.printf "Parking: %s\n" parking;
    List.iter (fun vol -> Printf.printf "  %s\n" vol.Vol.indicatif) vols
  ) ht



let calculer_intervalles_occupation (vols : Vol.t list) : unit =
  List.iter (fun (vol : Vol.t) ->
    if vol.type_vol = "ARR" then
      vol.occupation_parking <- { lower = vol.heure_parking; upper = vol.heure_parking + 15 }
    else if vol.type_vol = "DEP" then
      let heure_debut_int = int_of_string vol.heure_debut in
      vol.occupation_parking <- { lower = heure_debut_int - 15; upper = vol.dman }
    else
      vol.occupation_parking <- { lower = 0; upper = 0 }
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