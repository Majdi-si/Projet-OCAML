module Vol = Vol

let calculer_dman (vols : Vol.t list) : Vol.t list =
  List.map (fun (vol : Vol.t) -> 
    let dman = 
        let ttot = vol.ttot in
        let nombre_points = List.length vol.points in
        ttot - (5 * nombre_points) in
    { vol with dman }   
  ) vols