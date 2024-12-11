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

let liste_joueurs (_, l) = l
let quelle_couleur _ _ = Libre
let mis_a_jour_configuration conf _ = Ok (conf)
let gagnant _ = Libre
let coup_possible _ _ = []
(**)
let est_case ((i,j,k):case):bool=
  (i+j+k=0);;
let est_dans_losange ((i, j, k) : case) dim =
  (j <= dim) && (j >= -dim) &&
  (k <= dim) && (k >= -dim) &&
  (i + j + k = 0);;

(* fonctions tests de est_dans_losange *)
let()=
  let case = (0, 0, 0) in
  if est_dans_losange case dim then
    Printf.printf "La case est dans le losange"
  else
    Printf.printf "La case n'est pas dans le losange";;


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
let()= 
  let case = (0, 0, 0) in
  if est_dans_etoile case dim then
    Printf.printf "La case est dans l'etoile"
  else
    Printf.printf "La case n'est pas dans l'etoile";;

let quelle_couleur c (config, _) =
  if not (est_dans_etoile c dim) then
    Dehors
  else
    match List.assoc_opt c config with
    | None -> Libre
    | Some couleur -> couleur

let tourne_case m c =
  let (i, j, k) = c in
    match m mod 6 with
    | 0 -> (i, j, k)
    | 1 | -5 -> (-k, -i, -j)
    | 2 | -4 -> (j, k, i)
    | 3 | -3 -> (-i, -j, -k)
    | 4 | -2 -> (k, i, j)
    | 5 | -1 -> (-j, -k, -i)
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
  
let rec der_liste l =
  match l with
  | [] -> failwith "Liste vide : pas de dernier élément"
  | [a] -> a 
  | _ :: z -> der_liste z;;

let rec remplir_segment m (i, j, k) =
  if m <= 0 then []
  else
    (i, j, k) :: remplir_segment (m - 1) (i, j + 1, k - 1);;

let rec remplir_triangle_bas m (i, j, k) =
  if m <= 0 then []
  else
    remplir_segment m (i, j, k) @ remplir_triangle_bas (m - 1) (i - 1, j + 1, k);;

let rec remplir_triangle_haut m (i, j, k) =
  if m <= 0 then []
  else
    remplir_segment m (i, j, k) @ remplir_triangle_haut (m - 1) (i + 1, j, k - 1);;

let rec colorie (j : couleur) (liste : case list) : case_coloree list=
  match liste with 
    | [] -> []
    | t :: q -> [(t,j)] @ colorie j q;;

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
      let (cases_restantes, lj_restantes) = remplir_init_3 q dim (n-2) in
      (cases_colorees @ cases_restantes,lj);;

let rec remplir_init_6 (lj : couleur list) dim (n : int) : configuration =
  match lj with
  | [] -> ([],[])
  | t::q -> 
      let triangle = remplir_triangle_bas dim (-dim-1,1,dim) in
      let cases_tournees = List.map (tourne_case n) triangle in
      let cases_colorees = colorie t cases_tournees in
      let (cases_restantes, lj_restantes) = remplir_init_6 q dim (n-1) in
      (cases_colorees @ cases_restantes,lj);;

let remplir_init (lj : couleur list) dim : configuration =
  if List.length lj = 2 then remplir_init_2 lj dim
  else if List.length lj = 3 then remplir_init_3 lj dim 0
  else if List.length lj = 6 then remplir_init_6 lj dim 0
  else ([],[]);;

let configuration_initial = remplir_init [] dim;;

let est_case ((i,j,k):case):bool=
  (i+j+k=0);;

let case_libre (c:case) (conf:configuration):bool=
  quelle_couleur c conf = Libre;;


let supprime_dans_config ((cases, couleurs) : configuration) (c : case) : configuration =
  let cases_filtrees = List.filter (fun (case, _) -> case <> c) cases in
  (cases_filtrees, couleurs)

let rec est_libre_seg c1 c2 conf =
  let ((iv, jv, kv : vecteur) , d) = vec_et_dist c1 c2 in
  let rec aux (i, j, k) dist =
    if dist = 0 then true
    else
      let next_case = (i + iv, j + jv, k + (kv)) in
      if quelle_couleur next_case conf <> Libre then false
      else aux next_case (dist - 1)
  in
  aux c1 d

let est_saut (c1 : case) (c2 : case) (conf : configuration) : bool =
  let (i1, j1, k1) = c1 in
  let (i2, j2, k2) = c2 in
  let case_intermediaire = ((i1 + i2) / 2, (j1 + j2) / 2, (k1 + k2) / 2) in
  sont_cases_voisines c1 c2 && 
  quelle_couleur c2 conf = Libre && 
  quelle_couleur case_intermediaire conf <> Libre

  let rec est_saut_multiple (l : case list) (conf : configuration):bool =
    match l with 
    |[] -> failwith "Pas de saut possible"
    |[a] -> est_case a
    |(x1,y1,z1)::(x2,y2,z2)::fin -> let (u,v,w),d = vec_et_dist (x1,y1,z1) (x2,y2,z2)  in 
        est_case (x1,y1,z1) && est_case (x2,y2,z2) 
        && (d = 2) 
        && (not (case_libre (x2+u,y2+v,z2+w) conf)) 
        && (case_libre (x2,y2,z2) conf) 
        && est_saut_multiple ((x2,y2,z2)::fin) conf;;

let est_coup_valide (config : configuration) (coup : coup) : bool =
  match coup with
  | Du (c1, c2) ->
      est_case c1 &&
      est_case c2 &&
      sont_cases_voisines c1 c2 &&
      quelle_couleur c1 config = List.hd (snd config) &&
      quelle_couleur c2 config = Libre &&
      est_dans_losange c2 dim
  | Sm cases ->
      est_saut_multiple cases config &&
      est_dans_losange (der_liste cases) dim;;

let applique_coup (config : configuration) (coup : coup) : configuration =
  match coup with
  | Sm cases ->
      if not (est_coup_valide config coup) then
        failwith "Coup invalide"
      else
        let rec appliquer_saut_multiple cases conf =
          match cases with
          | [] -> conf
          | [c] -> conf 
          | c1 :: c2 :: rest ->
              let couleur = quelle_couleur c1 conf in
              let conf_sans_c1 = supprime_dans_config conf c1 in
              let (cases, couleurs) = conf_sans_c1 in
              let nouvelle_conf = ((c2, couleur) :: cases, couleurs) in
              appliquer_saut_multiple (c2 :: rest) nouvelle_conf
        in
        appliquer_saut_multiple cases config
  | Du (c1, c2) ->
      if not (est_coup_valide config coup) then
        failwith "Coup invalide"
      else
        let couleur = quelle_couleur c1 config in
        let config_sans_c1 = supprime_dans_config config c1 in
        let (cases, couleurs) = config_sans_c1 in
        ((c2, couleur) :: cases, couleurs)

let mis_a_jour_configuration (conf : configuration) (coup : coup) =
  match coup with
  | Sm _ ->
      if not (est_coup_valide conf coup) then
        Error "Coup invalide pour saut multiple"
      else
        Ok (applique_coup conf coup)
  | Du (c1, c2) ->
      if not (est_dans_etoile c2 dim) then
        Error "La case d'arrivée n'est pas dans l'étoile"
      else if quelle_couleur c1 conf <> List.hd (snd conf) then
        Error "Ce n'est pas votre pion"
      else if quelle_couleur c2 conf <> Libre then
        Error "La case d'arrivée n'est pas libre"
      else if not (sont_cases_voisines c1 c2) then
        Error "Les cases ne sont pas voisines"
      else
        try
          Ok (applique_coup conf coup)
        with
          | Failure msg -> Error msg


let gagnant _ = false
let coup_possible _ _ = []
let ia_next_coup _ = 
  let coup = Sm [] in
  let description = "Coup généré par ia_next_coup" in
  (coup, description);;
