

module TTOT = struct
  (* Module TTOT implementation *)
end

let calculer_dman (vols : t list) : t list =
  List.map (fun (vol : t) -> 
    let dman = 
        let ttot = vol.ttot in
        let nombre_points = List.length vol.points in
        ttot - (5 * nombre_points) in
    { vol with ttot = dman }   
  ) vols

let calculer_heure_parking (vols : t list) : t list =
  List.map (fun (vol : t) -> 
    let heure_parking = 
        let heure_piste_int = int_of_string vol.heure_piste in
        let nombre_points = List.length vol.points in
        heure_piste_int + (5 * nombre_points)
    in
    { vol with heure_piste = string_of_int heure_parking }   
  ) vols

let nbre_creneaux_rates (vols : t list) : int =
  (* Implementation of nbre_creneaux_rates *)
  0