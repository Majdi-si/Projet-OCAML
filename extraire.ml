module PF = Vol

(* Fonction qui extrait les informations et les stocke dans un tableau mutable *)
let rec extraire_info fichier tableau_ref =
  try
    (* Lire une ligne du fichier *)
    let ligne = input_line fichier in
    (* Séparer la ligne en champs en utilisant un espace comme séparateur *)
    let champs = String.split_on_char ' ' ligne in
    (* Vérifier que la ligne contient suffisamment de champs *)
    if List.length champs >= 8 then
      let type_vol = match List.nth champs 0 with
        | "DEP" -> PF.DEP
        | "ARR" -> PF.ARR
        | _ -> failwith "Type de vol invalide" in
      let indicatif = List.nth champs 1 in
      let turbulence = match List.nth champs 2 with
        | "H" -> PF.H
        | "M" -> PF.M
        | "L" -> PF.L
        | _ -> failwith "Turbulence invalide" in
      let parking = List.nth champs 3 in
      let qfu = List.nth champs 4 in
      let heure_debut = int_of_string (List.nth champs 5) in
      let heure_piste = int_of_string (List.nth champs 6) in
      let creneau = int_of_string (List.nth champs 7) in
      let points = PF.drop 8 champs in
      let etot = 0 in
      let ttot = 0 in
      (* Créer un enregistrement vol *)
      let vol = { PF.type_vol; indicatif; turbulence; parking; qfu; heure_debut; heure_piste; creneau; points; etot; ttot } in
      tableau_ref := vol :: !tableau_ref;
      extraire_info fichier tableau_ref
    else
      extraire_info fichier tableau_ref
  with
    | End_of_file -> ()
    | Failure msg -> failwith msg