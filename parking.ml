module Vol = Vol
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


let remplir_hashtbl (vols : Vol.t list) (liste_parkings : string list) (ht : Vol.t list ParkingHashtbl.t) : unit =
  List.iter (fun parking -> ParkingHashtbl.add ht parking []) liste_parkings;
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


