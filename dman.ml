module Vol = Vol

let calculer_dman (vols : Vol.t list) : Vol.t list =
  List.map (fun (vol : Vol.t) -> 
    let dman = 
        let ttot = vol.ttot in
        let nombre_points = List.length vol.points in
        ttot - (5 * nombre_points) in
    { vol with dman }   
  ) vols

let calculer_heure_parking (vols : Vol.t list) : Vol.t list =
  List.map (fun (vol : Vol.t) -> 
    let heure_parking = 
        let heure_piste_int = int_of_string vol.heure_piste in
        let nombre_points = List.length vol.points in
        heure_debut_int + (5 * nombre_points)
    in
    { vol with heure_parking }   
  ) vols