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

