module Vol = Vol

type sequence = {
  vols : Vol.t list;
  cout : int;
}

(* Fonction pour calculer le retard d'un vol placé après une séquence *)
let calculer_retard (vol : Vol.t) (seq : Vol.t list) =
  match seq with
  | [] -> 0
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
    let cat = vol.Vol.turbulence in
    if not (Hashtbl.mem categories cat) then
      Hashtbl.add categories cat vol
  ) vols;
  categories

(* Fonction principale d'optimisation *)
let optimiser_sequence (vols : Vol.t list) =
  let best = ref { vols = []; cout = max_int } in
  let max_iterations = 1000000000 in
  let iteration_count = ref 0 in
  List.iter (fun vol ->
    vol.Vol.ttot <- vol.Vol.etot;
  ) vols;
  
  let rec explorer seq cout reste =
    if !iteration_count >= max_iterations then
      Printf.printf "Limite d'itérations atteinte\n"
    else if cout < !best.cout then (
      incr iteration_count;
      match reste with
      | [] -> 
          best := { vols = List.rev seq; cout = cout };
      | _ ->
          let categories = grouper_par_categorie reste in
          Hashtbl.iter (fun _ vol ->
                let reste_r = List.filter ((<>) vol) reste in
                let cout_r = calculer_retard vol seq in
                let seq_r = vol :: seq in
                vol.ttot <- vol.etot + cout_r;
                explorer seq_r (cout + cout_r) reste_r;
                vol.ttot <- vol.etot

          ) categories
    )
  in

  let vols_depart = List.filter (fun v -> v.Vol.type_vol = "DEP") vols in
  let vols_tries = List.sort (fun v1 v2 -> compare v1.Vol.etot v2.Vol.etot) vols_depart in
  Printf.printf "Optimisation de %d vols de départ...\n" (List.length vols_tries);
  explorer [List.hd vols_tries] 0 (List.tl vols_tries);
  !best

let afficher_sequence (seq : sequence) =
  Printf.printf "Séquence optimisée (coût total: %d):\n" seq.cout;
  List.iter (fun vol ->
    Printf.printf "Vol %s (cat: %s, ETOT: %d)\n" 
      vol.Vol.indicatif vol.Vol.turbulence vol.Vol.etot
  ) seq.vols

let optimiser_jusqu_a_heure vols heure_cible piste =
  (* Fonction récursive auxiliaire pour traiter chaque heure *)
  let rec process_hours dernier_vol resultat h =
    if h > heure_cible then 
      resultat
    else
      (* Filtrer les vols de l'heure courante *)
      let vols_heure = List.filter (fun v -> 
        let heure = v.Vol.etot / 3600 in
        heure = h && v.Vol.type_vol = "DEP" && v.Vol.qfu = piste
      ) vols in
      
      if List.length vols_heure = 0 then
        (* Pas de vols cette heure, passer à l'heure suivante *)
        process_hours dernier_vol resultat (h + 1)
      else begin
        (* Mettre à jour les ETOT selon le dernier vol *)
        (match dernier_vol with
        | Some last_vol -> 
            List.iter (fun v -> 
              let separation = Ttot.separation_time last_vol.Vol.turbulence v.Vol.turbulence in
              v.Vol.etot <- max v.Vol.etot (last_vol.Vol.ttot + separation)
            ) vols_heure
        | None -> ());

        (* Optimiser la séquence *)
        let sequence = optimiser_sequence vols_heure in
        
        (* Mettre à jour les TTOT *)
        List.iter (fun v -> v.Vol.ttot <- v.Vol.etot) sequence.vols;
        
        (* Garder le dernier vol pour l'itération suivante *)
        let nouveau_dernier = Some (List.hd (List.rev sequence.vols)) in
        let nouveau_resultat = if h = heure_cible then Some sequence else resultat in
        
        process_hours nouveau_dernier nouveau_resultat (h + 1)
      end
  in
  
  process_hours None None 0

let optimiser_piste vols piste =
  let rec process_hours acc h =
    if h >= 24 then
      acc
    else 
      let resultat = optimiser_jusqu_a_heure vols h piste in
      process_hours (Array.set acc h resultat; acc) (h + 1)
  in
  process_hours (Array.make 24 None) 0

let afficher_resultats_piste piste resultats =
  Printf.printf "\n=== Résultats pour la piste %s ===\n" piste;
  Array.iteri (fun h res ->
    match res with
    | Some sequence -> 
        Printf.printf "\nHeure %d:\n" h;
        Printf.printf "Nombre de départs : %d\n" (List.length sequence.vols);
        Printf.printf "Retard total : %.2f minutes\n" 
          (float_of_int sequence.cout /. 60.0);
        Printf.printf "Retard moyen : %.2f minutes\n" 
          (float_of_int sequence.cout /. float_of_int (List.length sequence.vols) /. 60.0)
    | None -> 
        Printf.printf "\nHeure %d: Aucun vol\n" h
  ) resultats

let afficher_resultats_sequence resultat =
  match resultat with
  | Some sequence -> 
      Printf.printf "\nSéquence optimisée:\n";
      Printf.printf "Nombre de vols : %d\n" (List.length sequence.vols);
      Printf.printf "Retard total : %.2f minutes\n" (float_of_int sequence.cout /. 60.0);
      Printf.printf "Retard moyen : %.2f minutes\n" 
        (float_of_int sequence.cout /. float_of_int (List.length sequence.vols) /. 60.0);
      List.iter (fun vol ->
        Printf.printf "Vol %s (cat: %s) - ETOT: %d, TTOT: %d\n"
          vol.Vol.indicatif vol.Vol.turbulence vol.Vol.etot vol.Vol.ttot
      ) sequence.vols
  | None -> 
      Printf.printf "Aucune séquence optimisée disponible pour cette heure\n"