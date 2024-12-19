module Etot = Etot
module Vol = Vol

type category = L | M | H

let separation_time cat1 cat2 =
  match (cat1, cat2) with
  | ("M", "L") | ("H", "L") | ("H", "M") -> 180
  | ("H", "H") -> 120
  | _ -> 60

  let calculer_ttot (vols : Vol.t list) : Vol.t list =
    let rec aux (vols : Vol.t list) (last : Vol.t option) : unit =
      match vols with
      | [] -> ()
      | vol::tl -> (
          match last with
          | None -> vol.ttot <- vol.etot
          | Some v ->
            let sep = separation_time v.turbulence vol.turbulence in
            vol.ttot <- max vol.etot (v.ttot + sep));
          aux tl (Some vol) in
    aux vols None; vols

let afficher_info_ttot (vols : Vol.t list) =
  List.iter (fun (vol : Vol.t) ->
    Printf.printf "TTOT: %d\n" vol.ttot
  ) vols

let calcul_retard (vols : Vol.t list) =
  let total_diff = List.fold_left (fun acc (vol : Vol.t) ->
    acc + (vol.ttot - vol.etot)
  ) 0 vols in
  Printf.printf "Total difference between TTOT and ETOT: %d\n" total_diff

  let nb_creneaux_rate (vols : Vol.t list) =
    let count = List.fold_left (fun acc (vol : Vol.t) ->
      if vol.ttot > vol.etot + 600 then acc + 1 else acc
    ) 0 vols in
    Printf.printf "Nombre de créneaux ratés: %d\n" count;
    Printf.printf "Pourcentage de créneaux ratés: %.2f%%\n" (float_of_int count /. float_of_int (List.length vols) *. 100.0);
    Printf.printf "Vols ratés: ";
    List.iter (fun (vol : Vol.t) ->
      if vol.ttot > vol.etot + 600 then
        Printf.printf "%s " vol.indicatif
    ) vols;
    Printf.printf "\n"


let retard_moyen_par_heure (vols : Vol.t list) =
  let heures = Array.make 24 (0, 0) in
  List.iter (fun (vol : Vol.t) ->
    let heure = vol.etot / 3600 in
    let (total_retard, count) = heures.(heure) in
    heures.(heure) <- (total_retard + (vol.ttot - vol.etot), count + 1)
  ) vols;
  Array.iteri (fun heure (total_retard, count) ->
    if count > 0 then
      Printf.printf "Heure %02d: Retard moyen = %.2f minutes\n" heure (float_of_int total_retard /. float_of_int count /. 60.0)
    else
      Printf.printf "Heure %02d: Aucun vol\n" heure
  ) heures

let retard_moyen_global (vols : Vol.t list) =
  let total_retard, count = List.fold_left (fun (acc_retard, acc_count) (vol : Vol.t) ->
    (acc_retard + (vol.ttot - vol.etot), acc_count + 1)
  ) (0, 0) vols in
  if count > 0 then
    Printf.printf "Retard moyen global = %.2f minutes\n" (float_of_int total_retard /. float_of_int count /. 60.0)
  else
    Printf.printf "Aucun vol\n"