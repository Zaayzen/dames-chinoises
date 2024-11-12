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

let est_dans_etoile ((i, j, k) : case) dim =
  (* union des 3 losanges *)
  ((j <= dim) && (j >= -dim) && (k <= dim) && (k >= -dim) && (i + j + k = 0)) ||
  ((i <= dim) && (i >= -dim) && (k <= dim) && (k >= -dim) && (i + j + k = 0)) ||
  ((i <= dim) && (i >= -dim) && (j <= dim) && (j >= -dim) && (i + j + k = 0));;

let quelle_couleur (case : case) (configuration : configuration) : couleur =
  let (cases, _) = configuration in
  if est_dans_etoile case (dim) then Libre
  else Dehors;; 

let tourne_case m ((i, j, k): case) =
  let rec rotate_case m (i, j, k) =
    if m = 0 then (i, j, k)
    else rotate_case (m - 1) (-k, -i, -j) 
      in rotate_case m (i, j, k);;

let translate (c1, c2, c3) (v1, v2, v3) : case =
  (c1 + v1, c2 + v2, c3 + v3);;

let diff_case (c1a, c2a, c3a) (c1b, c2b, c3b) : case =
  (c1a - c1b, c2a - c2b, c3a - c3b);;

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

let()= 
  let case1 = (0, 0, 0) in
  let case2 = (1, 0, -1) in
  if(sont_cases_voisines case1 case2) then
    Printf.printf "Les cases sont voisines"
  else
    Printf.printf "Les cases ne sont pas voisines";;


let calcul_pivot (c1 : case) (c2 : case) : case option =
  let (c1a, c1b, c1c) = c1 in
  let (c2a, c2b, c2c) = c2 in
  if (c1a = c2a && (c1b + c2b) mod 2 = 0 && (c1c + c2c) mod 2 = 0) then
    Some (c1a, (c1b + c2b) / 2, (c1c + c2c) / 2)
  else if (c1b = c2b && (c1a + c2a) mod 2 = 0 && (c1c + c2c) mod 2 = 0) then
    Some ((c1a + c2a) / 2, c1b, (c1c + c2c) / 2)
  else if (c1c = c2c && (c1a + c2a) mod 2 = 0 && (c1b + c2b) mod 2 = 0) then
    Some ((c1a + c2a) / 2, (c1b + c2b) / 2, c1c)
  else
    None;;

let()=
  let case1 = (0, 2, -2) in
  let case2 = (0, -2, 2) in
  match calcul_pivot case1 case2 with
  | Some (c1, c2, c3) -> Printf.printf "Le pivot est (%d, %d, %d)\n" c1 c2 c3
  | None -> Printf.printf "Pas de pivot\n";;



let vec_et_dist (c1 : case) (c2 : case) : vecteur * int =
  let (dx, dy, dz) = diff_case c1 c2 in
  let d = (abs dx + abs dy + abs dz) / 2 in
  let unit_vector = (dx / d, dy / d, dz / d) in
  (unit_vector, d);;

let()=
  let c1 = (0,2,-2) in
  let c2 = (0,0,0) in
  let ((i, j, k : vecteur), d) = vec_et_dist c1 c2 in
  Printf.printf "Vecteur: (%d,%d,%d), Distance : %d\n" i j k d;;
