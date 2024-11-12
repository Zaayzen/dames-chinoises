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

(*let sont_cases_voisines (c1a, c2a, c3a) (c1b, c2b, c3b) : case =
  (* j'avoue c'est horrible mais flm de faire mieux c bon Ã§a marche nsm *)
  if (((c1a = c1b) && ((c2a = c2b + 1) || (c2a = c2b -1)) && ((c3a = c3b + 1) || (c3a = c3b -1)) && (c1a + c2a + c3a = 0) && (c1b + c2b + c3b = 0)) ||
      ((c2a = c2b) && ((c1a = c1b + 1) || (c1a = c1b -1)) && ((c3a = c3b + 1) || (c3a = c3b -1)) && (c1a + c2a + c3a = 0) && (c1b + c2b + c3b = 0)) ||
      ((c3a = c3b) && ((c2a = c2b + 1) || (c2a = c2b -1)) && ((c1a = c1b + 1) || (c1a = c1b -1)) && (c1a + c2a + c3a = 0) && (c1b + c2b + c3b = 0))) then true
  else false;; *)