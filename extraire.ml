(* Fonction qui extrait les informations et les stocke dans un tableau mutable (je reprends la meme fonction extraire_info)*)
let rec extraire_info fichier tableau_ref =
  try
    (* Lire une ligne du fichier *)
    let ligne = input_line fichier in
    (* Séparer la ligne en champs en utilisant un espace comme séparateur *)
    let champs = String.split_on_char ' ' ligne in
    (* Vérifier que la ligne contient suffisamment de champs *)
    if List.length champs >= 8 then
      let type_vol = List.nth champs 0 in
      let indicatif = List.nth champs 1 in
      let turbulence = List.nth champs 2 in
      let parking = List.nth champs 3 in
      let qfu = List.nth champs 4 in
      let heure_debut = List.nth champs 5 in
      let heure_piste = List.nth champs 6 in
      let creneau = List.nth champs 7 in
      let points = drop 8 champs in
      let etot = 0 in
      (* Créer un enregistrement vol *)
      let vol_info = {type_vol; indicatif; turbulence; parking; qfu; heure_debut; heure_piste; creneau; points; etot} in
      (* Ajouter l'enregistrement au tableau mutable *)
      tableau_ref := Array.append !tableau_ref [|vol_info|];
      (* Appel récursif pour traiter la ligne suivante *)
      extraire_info fichier tableau_ref
    else
      (* Si la ligne est mal formée, on l'ignore et on continue à lire le fichier *)
      extraire_info fichier tableau_ref
  with
    (* Gérer la fin du fichier *)
    End_of_file -> !tableau_ref  (* Retourner le tableau accumulé *)
