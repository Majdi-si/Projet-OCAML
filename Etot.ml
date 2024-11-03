(* Fonction pour calculer l'ETOT*)
let calculer_etot liste = 
 list.map(fun(type_vol, indicatif, turbulence, parking, qfu, heure_debut, heure_piste, creneau, points)-> 
 let etot = 
      if type_vol = "dep" then
        let heure_debut_int = int_of_string heure_debut in
        let nombre_points = List.length points in
        heure_debut_int + (5 * nombre_points)
      else
       None
    in
    (type_vol, indicatif, turbulence, parking, qfu, heure_debut, heure_piste, creneau, points, etot)
    )liste

(* Fonction pour afficher la liste avec ETOT *)
let afficher_info_etot liste =
  List.iter (fun (type_vol, indicatif, turbulence, parking, qfu, heure_debut, heure_piste, creneau, points, etot) ->
    Printf.printf "%s %s %s %s %s %s %s %s ETOT: %d\n" type_vol indicatif turbulence parking qfu heure_debut heure_piste creneau etot;
    List.iter (fun point -> Printf.printf "%s " point) points;
    Printf.printf "\n"
    )liste