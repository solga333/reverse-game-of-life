type state = Dead | Alive | Undefined (* État d'une cellule *)
type grid = state array array         (* Grille d'états *)

(* Direction pour le parcours en sous-matrices croissantes *)
type patternDirection =
  | Right
  | Down                              

exception NoGridLeft

(* Construit une grille indéterminée de côté n *)
let createUndefinedGrid (n : int) : grid =
  Array.make_matrix n n Undefined

(* state -> int (Dead=0, Alive=1, Undefined -> exception) *)
let stateToInt (state : state) : int =
  match state with
  | Dead -> 0
  | Alive -> 1
  | Undefined -> failwith "State observed is Undefined.\n"

(* state -> int sans erreur *)
let stateToIntNoError (state : state) : int =
  match state with
  | Dead -> 0
  | Alive -> 1
  | Undefined -> 9 (* valeur > 8 pour faire échouer les autres fonctions *)


(* Affiche une grille donnée dans le terminal *)
let showGrid (g : grid) : unit =
  let n = Array.length g in
  for i = 0 to n - 1 do
    Printf.printf "[ ";
    for j = 0 to n - 1 do
      Printf.printf "%d " (stateToIntNoError g.(i).(j))
    done;
    Printf.printf "]\n"
  done

(* Affiche un tableau de grilles dans le terminal puis le total *)
let showAllGrids (t : grid array) : unit =
  let n = Array.length t in
  if n = 0 then Printf.printf "\nAucune grille disponible.\n"
  else (
    for i = 0 to n - 1 do
      Printf.printf "\nGrille #%d : \n" (i + 1);
      showGrid t.(i)
    done;
    Printf.printf "\nTotal : %d grille(s) affichée(s).\n" n
  )

(** Affiche que le nombre total de grilles *)
let showNumberOfGrids (t : grid array) : unit =  
  let n = Array.length t in
  Printf.printf "\nTotal : %d grille(s).\n" n


(* Ajoute des bordures à une grille d'entiers *)
let addIntBorders (g : int array array) : int array array =
  let n = Array.length g in
  let c = Array.make_matrix (n + 2) (n + 2) 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      c.(i + 1).(j + 1) <- g.(i).(j)
    done
  done;
  c

(* Trouve le nombre de voisins vivants d'une cellule *)
let numberOfAliveNeighbors (g : grid) (i : int) (j : int) : int =
  let n = Array.length g in
  let count = ref 0 in
  let has_undefined = ref false in
  for di = -1 to 1 do
    for dj = -1 to 1 do
      if di <> 0 || dj <> 0 then (
        let ni = i + di in
        let nj = j + dj in
        if ni >= 0 && ni < n && nj >= 0 && nj < n then (
          match g.(ni).(nj) with
          | Alive -> incr count
          | Dead -> ()
          | Undefined -> has_undefined := true
        )
      )
    done
  done;
  if !has_undefined then 9 else !count

(* Donne la liste des états des voisins d'une cellule *)
let listOfNeighborsState (g : int array array) (i : int) (j : int) : int list =
  [
    g.(i - 1).(j - 1);
    g.(i).(j - 1);
    g.(i + 1).(j - 1);
    g.(i - 1).(j);
    g.(i + 1).(j);
    g.(i - 1).(j + 1);
    g.(i).(j + 1);
    g.(i + 1).(j + 1);
  ]

let maxList (l : int list) : int =
  List.fold_left (fun acc x -> if x > acc then x else acc) 0 l

(* Parcours linéaire classique *)
let linearPath (n : int) : (int * int) array =
  Array.init (n * n) (fun k -> (k / n, k mod n))

(* Parcours de sous-matrices croissantes *)
(* Note : ce parcours est plus rapide car il permet d'élaguer + tôt *)
let expandingSquareTraversal (n : int) : (int * int) array =
  let coords = Array.make (n * n) (0, 0) in
  let state = ref Down in
  let i, j, k = (ref 0, ref 0, ref 0) in
  while !i < n && !j < n do
    coords.(!k) <- (!i, !j);
    incr k;
    if !i = !j then (
      incr i;
      j := 0;
      state := Right
    ) else if !j + 1 = !i then (
      j := !i;
      i := 0;
      state := Down
    ) else if !state = Right then
      incr j
    else
      incr i
  done;
  coords

(* Pour un parcours donné et chaque cellule,
 calcule l'étape où elle-même et tous ses voisins ont été visités. *)
let generateVerificationMatrix (n : int) (coords : (int * int) array) : int array array =
  let order = Array.make_matrix n n 0 in
  Array.iteri (fun step (i, j) -> order.(i).(j) <- step) coords;
  let parc = addIntBorders order in
  let mat = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let neighbors = listOfNeighborsState parc (i + 1) (j + 1) in
      mat.(i).(j) <- maxList (order.(i).(j) :: neighbors)
    done
  done;
  mat

(* Vérifie si l'état de la cellule choisie est cohérente avec la grille cible *)
let verifyCellWithCurrentState (g : grid) (i : int) (j : int) (ef : state) : bool =
  let s = numberOfAliveNeighbors g i j in
  if s >= 9 then
    (* Au moins un voisin est encore indéfini : on conserve la branche *)
    true
  else
    match (g.(i).(j), ef) with
    | Dead, Alive -> s = 3
    | Alive, Dead -> s <> 2 && s <> 3
    | Alive, Alive -> s = 2 || s = 3
    | Dead, Dead -> s <> 3
    | Undefined, _ -> true
    | _, Undefined -> failwith "The target cell is undefined.\n"

(* Remplace chaque matrice par deux nouvelles avec 
l'état de la cellule choisie défini comme Dead ou Alive *)
let createNextPossibleGrids (arr : grid array) (i : int) (j : int) : grid array =
  let nb_matrices = Array.length arr in
  if nb_matrices = 0 then [||]
  else
    let next = Array.make (2 * nb_matrices) [||] in
    for k = 0 to nb_matrices - 1 do
      let copy_dead = Array.map Array.copy arr.(k) in
      copy_dead.(i).(j) <- Dead;
      next.(2 * k) <- copy_dead;

      let copy_alive = Array.map Array.copy arr.(k) in
      copy_alive.(i).(j) <- Alive;
      next.((2 * k) + 1) <- copy_alive
    done;
    next

(* Filtre les grilles qui ne passent pas le test de validation *)
let filterGrids (arr : grid array) (k : int) (l : int) (target : state) : grid array =
  let acc = ref [] in
  let len = Array.length arr in
  for x = len - 1 downto 0 do
    if verifyCellWithCurrentState arr.(x) k l target then
      acc := arr.(x) :: !acc
  done;
  Array.of_list !acc


(* Recherche d'antécédents avec un parcours passé en argument *)
let anteWithPath (pathFn : int -> (int * int) array) (finalGrid : grid) : grid array =
  let n = Array.length finalGrid in
  if n = 0 then [||]
  else
    try
      let coords = pathFn n in
      let verificationMatrix = generateVerificationMatrix n coords in
      let possibleGrids = ref [| createUndefinedGrid n |] in
      let matrixOfVerifiedCells = Array.make_matrix n n 1 in

      for step = 0 to (n * n) - 1 do
        let (i, j) = coords.(step) in
        if Array.length !possibleGrids = 0 then raise NoGridLeft;
        possibleGrids := createNextPossibleGrids !possibleGrids i j;

        (* Vérification de toutes les cellules prêtes à cette étape *)
        for k = 0 to n - 1 do
          for l = 0 to n - 1 do
            if
              matrixOfVerifiedCells.(k).(l) = 1
              && step >= verificationMatrix.(k).(l)
            then (
              matrixOfVerifiedCells.(k).(l) <- 0;
              possibleGrids := filterGrids !possibleGrids k l finalGrid.(k).(l);
              if Array.length !possibleGrids = 0 then raise NoGridLeft
            )
          done
        done
      done;
      !possibleGrids
    with NoGridLeft -> [||]

(* Fonction pour lancer directement avec le parcours par sous-matrices croissantes *)
let ante (finalGrid : grid) : grid array =
  anteWithPath expandingSquareTraversal finalGrid

let () =
  (* Test en 3x3 *)
  Printf.printf "Grille de côté 3\n";
  let g_3x3 =
    [|
      [| Dead; Alive; Dead |];
      [| Dead; Alive; Dead |];
      [| Dead; Alive; Dead |];
    |]
  in
  let sol_3x3 = ante g_3x3 in
  showAllGrids sol_3x3; (* 1 seule grille possible *)

  (* Test en 4x4 *)
  Printf.printf "\n\nGrille de côté 4\n";
  let g_4x4 =
    [|
      [| Dead; Alive; Dead; Dead |];
      [| Dead; Alive; Alive; Dead |];
      [| Alive; Dead; Alive; Alive |];
      [| Dead; Alive; Dead; Dead |];
    |]
  in
  let sol_4x4 = ante g_4x4 in
  showAllGrids sol_4x4; (* 1 seule grille possible *)

  (* Tests en 5x5 *)
  Printf.printf "\n\nGrille de côté 5 : Test 1\n";
  let g_5x5_m =
    [|
      [| Dead; Dead; Dead; Dead; Dead |];
      [| Dead; Dead; Dead; Dead; Dead |];
      [| Dead; Dead; Dead; Dead; Dead |];
      [| Dead; Dead; Dead; Dead; Dead |];
      [| Dead; Dead; Dead; Dead; Dead |];
    |]
  in
  let sol_5x5_m = ante g_5x5_m in
  showNumberOfGrids sol_5x5_m; (* 28 156 grilles possibles *)

  Printf.printf "\n\nGrille de côté 5 : Test 2\n";
  let g_5x5_eden =
    [|
      [| Dead; Dead; Alive; Dead; Dead |];
      [| Dead; Alive; Dead; Dead; Alive |];
      [| Dead; Alive; Dead; Dead; Alive |];
      [| Dead; Alive; Dead; Dead; Dead |];
      [| Alive; Dead; Alive; Alive; Alive |];
    |]
  in
  let sol_5x5_eden = ante g_5x5_eden in
  showAllGrids sol_5x5_eden (* 0 grilles possibles *)
