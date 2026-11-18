# Recherche de grilles antécédentes au jeu de la Vie de Conway

## Présentation du projet

Le **[Jeu de la Vie](https://fr.wikipedia.org/wiki/Jeu_de_la_vie)** est un automate cellulaire où l'état futur d'une cellule (vivante ou morte) dépend uniquement du nombre de voisins vivants autour d'elle. Si calculer l'étape suivante est immédiat, faire le chemin inverse est beaucoup plus difficile : **étant donnée une grille à l'instant $t+1$, quelles étaient les grilles possibles à l'instant $t$ ?**

Tester toutes les grilles possibles de manière naïve serait très long au vu de la complexité (le programme serait en $O(2^{n^2})$ pour une grille de taille $n \times n$).

Ce projet implémente donc un algorithme de recherche en **OCaml** capable de retrouver efficacement tous les antécédents d'une grille grâce à deux optimisations :
1. **L'élagage de l'arbre des possibilités (*Branch-and-Bound*)** : On fixe chaque cellule comme morte ou vivante les unes après les autres et dès qu'une cellule et tous ses voisins sont fixés, on vérifie immédiatement si la règle de Conway est respectée. Si ce n'est pas le cas, on élimine immédiatement toute cette branche de recherche sans perdre de temps à tester le reste de la grille.
2. **L'optimisation du parcours (en sous-matrices croissantes)** : au lieu de parcourir bêtement ligne par ligne, on fait grandir des carrés depuis le coin supérieur gauche (voir la fonction `expandingSquareTraversal`). Les voisinages se complètent beaucoup plus vite, ce qui permet d'éliminer les branches impossibles bien plus tôt.

Le programme permet également d'identifier des **Jardins d'Éden** : des configurations qui ont exactement 0 antécédent (qui ne peuvent être que des états initiaux).

## Descriptif du fichier

Le code `research.ml` contient :
  - La définition des types et la vérification des règles du Jeu de la Vie **avec bordures mortes**.
  - La génération automatique des matrices d'élagage (`generateVerificationMatrix`).
  - Le parcours par sous-matrices croissantes.
  - L'algorithme de recherche inverse (`ante`).
  - Quelques tests qui évaluent plusieurs critères (un test avec un seul antécédent, un avec +28000, un jardin d'Éden, etc.). 

## Dépendances

- `OCaml 5.2+` (notamment pour la fonction `Array.init_matrix`)

## Exécution

Vous pouvez exécuter le script directement avec l'interpréteur OCaml :

```bash
ocaml research.ml
```

Il est conseillé tout de même de compiler le code pour de meilleurs performances : 

```bash
ocamlopt -O3 research.ml -o research
./research
```
