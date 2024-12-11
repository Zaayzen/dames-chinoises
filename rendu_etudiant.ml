let dim = 3;;

type case = int * int * int;;

type vecteur = int * int * int;;

type couleur =
  | Vert
  | Jaune
  | Rouge
  | Noir
  | Bleu
  | Marron
  | Libre (*case libre du plateau*)
  | Dehors (*case en dehors du plateau, utile pour l'affichage*);;

let string_of_couleur x =
  match x with
  | Vert -> "Vert"
  | Jaune -> "Jaune"
  | Rouge -> "Rouge"
  | Noir -> "Noir"
  | Bleu -> "Bleu"
  | Marron -> "Marron"
  | Libre -> "Libre"
  | Dehors -> "Dehors";; 

type case_coloree = case * couleur;;
type configuration = case_coloree list * couleur list;;
type coup = Du of case * case | Sm of case list;;

let configuration_initial = ([], [ Vert; Jaune; Rouge; Noir; Bleu; Marron ]);;
let liste_joueurs (_, l) = l;;
let quelle_couleur _ _ = Libre;;
let mis_a_jour_configuration _ _ = Error "To do";;
let gagnant _ = Libre;;
let coup_possible _ _ = [];;
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


let tourne_case m c =
  let (i, j, k) = c in
  match m mod 6 with
  | 0 -> (i, j, k)
  | 1 | -5 -> (-k, -i, -j)
  | 2 | -4 -> (j, k, i)
  | 3 | -3 -> (-i, -k, -j)
  | 4 | -2 -> (k, j, i)
  | 5 | -1 -> (-j, -i, -k)
  | _ -> failwith "Impossible";;

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

let rec tourne_liste (l : couleur list) : couleur list =
  match l with
  |[] -> []
  |[a] -> [a]
  |t::q -> tourne_liste [] @ q @ [t];;
  
let rec der_liste (l : couleur list) : couleur =
  match l with
  | [] -> failwith "Liste vide : pas de dernier élément"
  | [a] -> a 
  | _ :: z -> der_liste z;;

let rec remplir_segement (m : int) (c : case) : case list =
  let (i,j,k) = c in
  if m = 0 then []
  else (i,j,k) :: remplir_segement (m-1) (i,j+1,k-1);;

let rec remplir_triangle_bas (m : int) (c : case) : case list =
  let (i,j,k) = c in
  if m = 0 then []
  else remplir_segement m c @ remplir_triangle_bas (m-1) (i-1,j+1,k);;

let rec remplir_triangle_haut (m : int) (c: case) : case list =
  let (i,j,k) = c in
  if m = 0 then[]
  else remplir_segement m c @ remplir_triangle_haut (m-1) (i+1,j,k-1);;

let rec colorie (coul : couleur) (lc : case list) : case_coloree list =
  match lc with
  |[] -> []
  |t :: q -> (t, coul) :: (colorie coul q);;

let tourne_config (cases, couleurs) =
  let n = List.length couleurs in
  let m = 6 / n in
  let nouvelles_cases = List.map (fun (c, coul) -> (tourne_case m c, coul)) cases in
  let nouvelles_couleurs = tourne_liste couleurs in
  (nouvelles_cases, nouvelles_couleurs);;

let remplir_init_2 (lj : couleur list) dim : configuration =
  let triangle1 = remplir_triangle_bas dim (-dim-1,1,dim) in
  let triangle2 = remplir_triangle_haut dim (dim+1,-dim,1) in
  let coul1 = der_liste (tourne_liste lj) in
  let coul2 = der_liste lj in 
  let cases_colorees = colorie coul1 triangle1 @ colorie coul2 triangle2 in
  (cases_colorees,lj);;

let rec remplir_init_3 (lj : couleur list) dim (n : int) : configuration =
  match lj with
  | [] -> ([],[])
  | t::q -> 
      let triangle = remplir_triangle_bas dim (-dim-1,1,dim) in
      let cases_tournees = List.map (tourne_case n) triangle in
      let cases_colorees = colorie t cases_tournees in
      let (cases_restantes, lj_restantes) = remplir_init_3 q dim (n+2) in
      (cases_colorees @ cases_restantes,lj);;

let rec remplir_init_6 (lj : couleur list) dim (n : int) : configuration =
  match lj with
  | [] -> ([],[])
  | t::q -> 
      let triangle = remplir_triangle_bas dim (-dim-1,1,dim) in
      let cases_tournees = List.map (tourne_case n) triangle in
      let cases_colorees = colorie t cases_tournees in
      let (cases_restantes, lj_restantes) = remplir_init_3 q dim (n+1) in
      (cases_colorees @ cases_restantes,lj);;

let remplir_init (lj : couleur list) dim : configuration =
  if List.length lj = 2 then remplir_init_2 lj dim
  else if List.length lj = 3 then remplir_init_3 lj dim 0
  else if List.length lj = 6 then remplir_init_6 lj dim 0
  else ([],[])

(*remplir init -> configuration
  confirguration = (case_coloree list,couleur list)
  colorie -> case_coloree list
  lj : couleur list
  case_coloree list = (case list,coul)
  remplir_triangle_bas -> case list*)
  
let rec quelle_couleur c (cases, _) =
  match List.assoc_opt c cases with
  | Some coul -> coul
  | None -> Libre;;
  
let supprime_dans_config ((cases, couleurs) : configuration) (c : case) : configuration =
  let nouvelles_cases = List.filter (fun (case, _) -> case <> c) cases in
  (nouvelles_cases, couleurs);;
  
let rec est_libre_seg (c1:case) (c2:case) (conf:configuration) : bool =
  let rec aux (c:case) (c2:case) =
    if c = c2 then true
    else
      let (v, _) = vec_et_dist c c2 in
      quelle_couleur c conf = Libre && aux (translate c v) c2
  in
  aux c1 c2;;
  
let est_saut (c1 : case) (c2 : case) (conf : configuration) : bool =
  let ((dx, dy, dz), dist) = vec_et_dist c1 c2 in
  dist = 2 && est_libre_seg c1 c2 conf;;
  
let est_saut_multiple (coup : coup) (conf : configuration) : bool =
  match coup with
  | Sm path when List.length path > 1 ->
      let valid = List.for_all (fun (c1, c2) -> est_saut c1 c2 conf) (List.tl (List.combine path (List.tl path))) in
      valid && est_libre_seg (List.hd path) (List.hd (List.rev path)) conf
  | _ -> false;;
 
let est_coup_valide (conf : configuration) (coup : coup) : bool =
  match coup with
  | Du (c1, c2) ->
    (* Vérifie si les cases c1 et c2 sont voisines *)
      let voisins c1 c2 =
        let (a, b, c) = c1 in
        let (a', b', c') = c2 in
        (abs (a - a') <= 1 && abs (b - b') <= 1 && abs (c - c') <= 1) in
  
      (* Vérifie si c1 contient un pion du joueur courant *)
      let joueur = quelle_couleur c1 conf in
      let est_joueur = function
        | Libre -> false
        | _ -> joueur = quelle_couleur c1 conf in
  
      (* Vérifie si c2 est libre et valide *)
      let est_libre c = quelle_couleur c conf = Libre in

      voisins c1 c2 && est_joueur joueur && est_libre c2 && (quelle_couleur c2 conf <> Dehors)
  | _ -> false;;
 
let applique_coup (conf : configuration) (coup : coup) : configuration =
  let (cases, couleurs) = conf in
  match coup with
  | Du (c1, c2) when est_coup_valide conf coup ->
      let updated_cases = List.map (fun (c, col) -> if c = c1 then (c2, col) else (c, col)) cases in
      (updated_cases, couleurs)
  | _ -> conf;;
  
let mis_a_jour_configuration (conf : configuration) (coup : coup) : (configuration, string) result =
  let (cases, couleurs) = conf in
  match coup with
  | Du (c1, c2) when est_coup_valide conf coup ->
      let updated_cases = List.map (fun (c, col) -> if c = c1 then (c2, col) else (c, col)) cases in
      Ok (updated_cases, couleurs)
  | Sm path when est_saut_multiple coup conf ->  (* Gestion des sauts multiples *)
      let rec aux conf path =
        match path with
        | [] -> conf
        | [c] -> conf  (* La case finale ne nécessite pas de changement *)
        | c1 :: c2 :: rest ->
            let new_conf = mis_a_jour_configuration conf (Du (c1, c2)) |> Result.get_ok in
            aux new_conf (c2 :: rest)
      in
      Ok (aux conf path)
  | Du _ -> Error "Ce coup n’est pas valide, la case d’arrivée est occupée."
  | _ -> Error "Saut multiples non implementés";;

let gagnant  = false;;
let coup_possible   = [];;
let ia_next_coup  = 
  let coup = Sm [] in
  let description = "Coup généré par ia_next_coup" in
  (coup, description);;