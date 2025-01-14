# Gestionnaire de Trafic Aérien en OCaml

## Description
Ce projet implémente un système de gestion du trafic aérien en OCaml, conçu pour optimiser les séquences de décollage et gérer l'occupation des parkings dans un aéroport. Il permet de :

- Calculer les horaires de décollage optimaux (TTOT)
- Détecter les conflits de parking
- Générer des statistiques détaillées

## Structure du Projet
Le projet est organisé en plusieurs modules :

- **Vol** : Définition du type vol et fonctions de base
- **Etot** : Calcul des temps estimés de décollage
- **Ttot** : Calcul des temps cibles de décollage
- **Parking** : Gestion des conflits de parking
- **Optimisation** : Algorithmes d'optimisation des séquences
- **Traitement_donnees** : Génération des statistiques
- **Parametres** : Configuration des paramètres et mise en place de l'interface utilisateur

## Utilisation

### Menu principal
1. Générer statistiques
2. Optimiser séquence
3. Analyser conflits
4. Modifier paramètres
5. Afficher paramètres
6. Changer fichier de données
7. Statistiques par piste
8. Analyser pics d'activité
9. Statistiques par catégorie
10. Quitter

## Format des données
Le fichier d'entrée doit contenir une ligne par vol avec le format spécifié.

Type de mouvement : ARR (Arrivée) ou DEP (Départ).
Identifiant unique de l’avion.
QFU : Orientation de la piste.
Catégorie de turbulence : L (Light), M (Medium), H (Heavy).
Parking assigné.
Horaires : Heure de début, heure de piste, créneau (en secondes depuis minuit).
Trajectoire : Coordonnées des points suivis.


## Statistiques
Les statistiques sont exportées au format CSV avec les colonnes suivantes :
- Heure
- Nombre de vols
- Retard moyen
- Retard total
- Nombre de vols en retard
- Conflits de parking
- Créneaux ratés
- Statistiques par piste
- Retards optimisés

## Configuration
Les paramètres configurables incluent :
- Temps d'occupation parking
- Temps de séparation entre avions selon leur catégorie de turbulence
- Choix des pistes à analyser


## Lancement de l'application

### Prérequis
- OCaml (version 4.13.0 ou supérieure)
- Dune build system
- Make

### Compilation 
Compiler le projet
```bash
make
```

### Exécution
Pour lancer l'application :
```bash
./main
```

Pour nettoyer les fichiers de compilation :
```bash
make clean
```

## Auteurs
- Papa Amadou Dieng
- Majdi Si-Salah
- Hadriens Sievers