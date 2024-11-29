(* Etot.ml *)
module Vol = Vol

let calculer_etot (vols : Vol.t list) : Vol.t list =
  List.map (fun (vol : Vol.t) -> 
    let etot = 
      if vol.type_vol = "DEP" then
        let heure_debut_int = int_of_string vol.heure_debut in
        let nombre_points = List.length vol.points in
        heure_debut_int + (5 * nombre_points)
      else
        0
    in
    { vol with etot }
  ) vols

let afficher_info_etot (vols : Vol.t list) =
  List.iter (fun (vol : Vol.t) ->
    Printf.printf "%s %s %s %s %s %s %s %s ETOT: %d\n" vol.type_vol vol.indicatif vol.turbulence vol.parking vol.qfu vol.heure_debut vol.heure_piste vol.creneau vol.etot;
    List.iter (fun point -> Printf.printf "%s " point) vol.points;
    Printf.printf "\n"
  ) vols

  for n in range of 0 to length tableau_ref