module Vol = Vol

type sequence = {
  vols : Vol.t list;
  cout : int;
}

(* Fonction pour calculer le retard d'un vol placé après une séquence *)
let calculer_retard (vol : Vol.t) (seq : Vol.t list) =
  match List.rev seq with
  | [] -> max 0 (vol.Vol.ttot - vol.Vol.etot)  
  | dernier :: _ ->
      let separation = Ttot.separation_time dernier.Vol.turbulence vol.Vol.turbulence in
      let nouveau_ttot = max vol.Vol.etot (dernier.Vol.ttot + separation) in
      nouveau_ttot - vol.Vol.etot

(* Fonction pour calculer le retard total d'une séquence *)
let calcul_retard_total (vols : Vol.t list) =
  let rec aux acc sequence = function
    | [] -> acc
    | vol :: reste ->
        let retard = calculer_retard vol sequence in
        aux (acc + retard) (sequence @ [vol]) reste
  in
  aux 0 [] vols

(* Fonction pour regrouper les vols par catégorie *)
let grouper_par_categorie (vols : Vol.t list) =
  let categories = Hashtbl.create 3 in 
  List.iter (fun vol ->
    let cat_vols = try Hashtbl.find categories vol.Vol.turbulence 
                   with Not_found -> [] in
    Hashtbl.replace categories vol.Vol.turbulence (vol :: cat_vols)
  ) vols;
  categories

(* Fonction principale d'optimisation *)
let optimiser_sequence (vols : Vol.t list) =
  let best = ref { vols = vols; cout = calcul_retard_total vols } in
  let max_iterations = 1000000 in
  let iteration_count = ref 0 in
  
  let rec explorer seq cout reste =
    if !iteration_count >= max_iterations then
      Printf.printf "Limite d'itérations atteinte\n"
    else if cout < !best.cout then (
      incr iteration_count;
      match reste with
      | [] -> 
          best := { vols = seq; cout = cout };
          Printf.printf "Nouvelle meilleure solution trouvée: coût %d\n" cout
      | _ ->
          let categories = grouper_par_categorie reste in
          Hashtbl.iter (fun _ cat_vols ->
            match cat_vols with
            | [] -> ()
            | vol :: _ ->
                let reste_r = List.filter ((<>) vol) reste in
                let cout_r = calculer_retard vol seq in
                let seq_r = seq @ [vol] in
                if cout + cout_r < !best.cout then 
                  explorer seq_r (cout + cout_r) reste_r
          ) categories
    )
  in

  let vols_depart = List.filter (fun v -> v.Vol.type_vol = "DEP") vols in
  let vols_tries = List.sort (fun v1 v2 -> compare v1.Vol.etot v2.Vol.etot) vols_depart in
  Printf.printf "Optimisation de %d vols de départ...\n" (List.length vols_tries);
  explorer [] 0 vols_tries;
  !best

let afficher_sequence (seq : sequence) =
  Printf.printf "Séquence optimisée (coût total: %d):\n" seq.cout;
  List.iter (fun vol ->
    Printf.printf "Vol %s (cat: %s, ETOT: %d)\n" 
      vol.Vol.indicatif vol.Vol.turbulence vol.Vol.etot
  ) seq.vols