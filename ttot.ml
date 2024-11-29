module Etot = Etot
module Vol = Vol

type category = L | M | H

let separation_time cat1 cat2 =
  match (cat1, cat2) with
  | ("L", "M") | ("L", "H") | ("M", "H") -> 3
  | ("H", "H") -> 2
  | _ -> 1

let calculer_ttot (vols : Vol.t list) : Vol.t list =
  let rec aux (vols : Vol.t list) (ttot : int) : Vol.t list =
    match vols with
    | [] -> []
    | [vol] -> [{vol with ttot}]
    | vol1 :: vol2 :: reste ->
      let ttot = 
        if vol1.type_vol = "DEP" && vol2.type_vol = "DEP" then
          let cat1 = vol1.turbulence in
          let cat2 = vol2.turbulence in
          let separation = separation_time cat1 cat2 in
          let etot1 = vol1.etot in
          let etot2 = vol2.etot in
          let ttot1 = etot1 + separation in
          let ttot2 = etot2 + separation in
          let ttot = max ttot1 ttot2 in
          ttot
        else
          0
      in
      {vol1 with ttot} :: aux (vol2 :: reste) ttot
  in
  aux vols 0

let afficher_info_ttot (vols : Vol.t list) =
  List.iter (fun (vol : Vol.t) ->
    Printf.printf "TTOT: %d\n" vol.ttot
  ) vols

let calcul_retard (vols : Vol.t list) =
  let total_diff = List.fold_left (fun acc (vol : Vol.t) ->
    acc + (vol.ttot - vol.etot)
  ) 0 vols in
  Printf.printf "Total difference between TTOT and ETOT: %d\n" total_diff