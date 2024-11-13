let dim = 3

type case = int * int * int

type vecteur = int * int * int

type couleur =
  | Vert
  | Jaune
  | Rouge
  | Noir
  | Bleu
  | Marron
  | Libre (*case libre du plateau*)
  | Dehors
(*case en dehors du plateau, utile pour l'affichage*)

let string_of_couleur x =
  match x with
  | Vert -> "Vert"
  | Jaune -> "Jaune"
  | Rouge -> "Rouge"
  | Noir -> "Noir"
  | Bleu -> "Bleu"
  | Marron -> "Marron"
  | Libre -> "Libre"
  | Dehors -> "Dehors"

type case_coloree = case * couleur
type configuration = case_coloree list * couleur list
type coup = Du of case * case | Sm of case list

let configuration_initial = ([], [ Vert; Jaune; Rouge; Noir; Bleu; Marron ])
let liste_joueurs (_, l) = l
let quelle_couleur _ _ = Libre
let mis_a_jour_configuration _ _ = Error "To do"
let gagnant _ = Libre
let coup_possible _ _ = []
(**)
let est_dans_losange ((i, j, k) : case) dim =
  (j <= dim) && (j >= -dim) &&
  (k <= dim) && (k >= -dim) &&
  (i + j + k = 0);;

let test_est_dans_losange (i, j, k) =
  if est_dans_losange (i,j,k) dim then
    Printf.printf "La case est dans le losange"
  else
    Printf.printf "La case n'est pas dans le losange";;

test_est_dans_losange (0,0,0);; (* renvoie La case est dans le losange *)
test_est_dans_losange (4,2,-6);; (* renvoie La case n'est pas dans le losange *)
test_est_dans_losange (33,23,-56);; (* renvoie La case n'est pas dans le losange *)
test_est_dans_losange (0,1,0);; (* renvoie La case n'est pas dans le losange *)
test_est_dans_losange (5,-4,-1);; (* renvoie La case n'est pas dans le losange *)
test_est_dans_losange (-5,-1,6);; (* renvoie La case n'est pas dans le losange *)
test_est_dans_losange (6,-1,-5);; (* renvoie La case n'est pas dans le losange *)


let est_dans_etoile ((i, j, k) : case) dim =
  (* union des 3 losanges *)
  (* 1ere facon de le faire
  ((j <= dim) && (j >= -dim) && (k <= dim) && (k >= -dim) && (i + j + k = 0)) ||
  ((i <= dim) && (i >= -dim) && (k <= dim) && (k >= -dim) && (i + j + k = 0)) ||
  ((i <= dim) && (i >= -dim) && (j <= dim) && (j >= -dim) && (i + j + k = 0));;
  *)
  (* amélioration de la fonction *)
  est_dans_losange (i, j, k) dim || est_dans_losange (j, k, i) dim || est_dans_losange (k, i, j) dim;;

(* fonctions tests de est_dans_etoile *)
let test_est_dans_etoile (i, j, k) =
  if est_dans_etoile (i,j,k) dim then
    Printf.printf "La case est dans le losange"
  else
    Printf.printf "La case n'est pas dans le losange";;

test_est_dans_etoile (0,0,0);; (* renvoie La case est dans l'etoile *)
test_est_dans_etoile (4,2,-6);; (* renvoie La case n'est dans l'etoile *)
test_est_dans_etoile (33,23,-56);; (* renvoie La case n'est pas dans l'etoile *)
test_est_dans_etoile (0,1,0);; (* renvoie La case n'est pas dans l'etoile *)
test_est_dans_etoile (5,-4,-1);; (* renvoie La case n'est dans l'etoile *)
test_est_dans_etoile (-5,-1,6);; (* renvoie La case n'est dans l'etoile *)
test_est_dans_etoile (6,-1,-5);; (* renvoie La case n'est dans l'etoile *)
test_est_dans_etoile (6,-3,-3);; (* renvoie La case est dans l'etoile *)
test_est_dans_etoile (-2,-3,5);; (* renvoie La case est dans l'etoile *)
test_est_dans_etoile (3,-5,2);; (* renvoie La case est dans l'etoile *)

let quelle_couleur (case : case) (configuration : configuration) : couleur =
  let (cases, _) = configuration in
  if est_dans_etoile case (dim) then Libre
  else Dehors;; 

(* fonctions tests de quelle_couleur *)
let()=
  let case = (0, 0, 0) in
  let configuration = ([], [ Vert; Jaune; Rouge; Noir; Bleu; Marron ]) in
  let couleur = quelle_couleur case configuration in
  Printf.printf "La couleur de la case est %s" (string_of_couleur couleur);;


let tourne_case m ((i, j, k): case) =
  let rec rotate_case m (i, j, k) =
    if m = 0 then (i, j, k)
    else rotate_case (m - 1) (-k, -i, -j) 
      in rotate_case m (i, j, k);;

(* fonctions tests de tourne_case *)
let()=
  let case = (0, 0, 0) in
  let m = 1 in
  let (i, j, k) = tourne_case m case in
  Printf.printf "La case tournee est (%d, %d, %d)" i j k;;


let translate ((c1, c2, c3): case) ((v1, v2, v3): vecteur) : case =
  (c1 + v1, c2 + v2, c3 + v3);;

(* fonctions tests de translate *)
let()=
  let case = (0, 0, 0) in
  let vecteur = (1, 1, 1) in
  let (i, j, k) = translate case vecteur in
  Printf.printf "La case translatee est (%d, %d, %d)" i j k;;


let diff_case ((c1a, c2a, c3a) : case) ((c1b, c2b, c3b) : case) : vecteur =
  (c1a - c1b, c2a - c2b, c3a - c3b);;

(* fonctions tests de diff_case *)
let()=
  let case1 = (0, 0, 0) in
  let case2 = (1, 1, 1) in
  let (i, j, k) = diff_case case1 case2 in
  Printf.printf "La difference des cases est (%d, %d, %d)" i j k;;


let sont_cases_voisines (case1 : case) (case2 : case) : bool =
  let (c1a, c2a, c3a) = case1  in
  let (c1b, c2b, c3b) = case2  in
  if (c1a, c2a, c3a) = (c1b, c2b + 1, c3b - 1) ||
    (c1a, c2a, c3a) = (c1b, c2b - 1, c3b + 1) ||
    (c1a, c2a, c3a) = (c1b + 1, c2b, c3b - 1) ||
    (c1a, c2a, c3a) = (c1b - 1, c2b, c3b + 1) ||
    (c1a, c2a, c3a) = (c1b - 1, c2b + 1, c3b) ||
    (c1a, c2a, c3a) = (c1b + 1, c2b - 1, c3b) then true
  else false;;

(* fonctions tests de sont_cases_voisines *)
let()= 
  let case1 = (0, 0, 0) in
  let case2 = (1, 0, -1) in
  if(sont_cases_voisines case1 case2) then
    Printf.printf "Les cases sont voisines"
  else
    Printf.printf "Les cases ne sont pas voisines";;


let calcul_pivot (case1 : case) (case2 : case) : case option =
  let (c1a, c2a, c3a) = case1 in
  let (c1b, c2b, c3b) = case2 in
  if (c1a = c1b && (c2a + c2b) mod 2 = 0 && (c3a + c3b) mod 2 = 0) then
    Some (c1a, (c2a + c2b) / 2, (c3a + c3b) / 2)
  else if (c2a = c2b && (c1a + c1b) mod 2 = 0 && (c3a + c3b) mod 2 = 0) then
    Some ((c1a + c1b) / 2, c2a, (c3a + c3b) / 2)
  else if (c3a = c3b && (c1a + c1b) mod 2 = 0 && (c2a + c2b) mod 2 = 0) then
    Some ((c1a + c1b) / 2, (c2a + c2b) / 2, c3a)
  else None;;

(* fonctions tests de calcul_pivot *)
let()=
  let case1 = (0, 2, -2) in
  let case2 = (0, -2, 2) in
  match calcul_pivot case1 case2 with
  | Some (c1, c2, c3) -> Printf.printf "Le pivot est (%d, %d, %d)\n" c1 c2 c3
  | None -> Printf.printf "Pas de pivot\n";;


let vec_et_dist (case1 : case) (case2 : case) : vecteur * int =
  let (dx, dy, dz) = diff_case case1 case2 in
  let d = (abs dx + abs dy + abs dz) / 2 in
  let unit_vector = (dx / d, dy / d, dz / d) in
  (unit_vector, d);;

(* fonctions tests de vec_et_dist *)
let()=
  let case1 = (0,2,-2) in
  let case2 = (0,0,0) in
  let ((i, j, k : vecteur), d) = vec_et_dist case1 case2 in
  Printf.printf "Vecteur: (%d,%d,%d), Distance : %d\n" i j k d;;