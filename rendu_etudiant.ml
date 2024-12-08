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

let rec tourne_liste l =
  match l with
  | [] -> [] 
  | [x] -> [x] 
  | x :: xs -> xs @ [x];;

let rec remplir_segment m (i, j, k) =
  if m <= 0 then []
  else
    (i, j, k) :: remplir_segment (m - 1) (i, j + 1, k - 1);;


let rec remplir_triangle_bas m (i, j, k) =
  if m <= 0 then [] 
  else
    let ligne_actuelle = remplir_segment m (i, j, k) in
    let triangle_restant = remplir_triangle_bas (m - 1) (i - 1, j + 1, k) in
    ligne_actuelle @ triangle_restant;;

let rec remplir_triangle_haut m (i, j, k) =
  if m <= 0 then []
  else
    let ligne_actuelle = remplir_segment m (i, j, k) in
    let triangle_restant = remplir_triangle_haut (m - 1) (i + 1, j, k - 1) in
    ligne_actuelle @ triangle_restant;;

let rec colorie col lc =
  match lc with
  | [] -> []
  | (i, j, k) :: rest -> (i, j, k, col) :: colorie col rest;;

let tourne_case (i, j, k) p =
  match p mod 6 with
  | 0 -> (i, j, k)
  | 1 -> (j, k, i)
  | 2 -> (k, i, j)
  | 3 -> (-i, -j, -k)
  | 4 -> (-j, -k, -i)
  | 5 -> (-k, -i, -j)
  | _ -> failwith "Invalid rotation";;

let tourne_config (cases, couleurs) =
  let n = List.length couleurs in
  let m = 6 / n in
  let nouvelles_cases = List.map (fun (c, coul) -> (tourne_case m c, coul)) cases in
  let nouvelles_couleurs = tourne_liste couleurs in
  (nouvelles_cases, nouvelles_couleurs);;

let remplir_init liste_joueurs dim =
  let rec aux joueurs acc_cases acc_couleurs =
    match joueurs with
    | [] -> (acc_cases, acc_couleurs)
    | coul::rest ->
      let triangle = remplir_triangle_bas dim (0, 0, 0) in
      let cases_colorees = colorie coul triangle in
      aux rest (acc_cases @ cases_colorees) (acc_couleurs @ [coul])
  in
  aux liste_joueurs [] [];;

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

let applique_coup (conf : configuration) (coup : coup) : configuration =
  let (cases, couleurs) = conf in
  match coup with
  | Du (c1, c2) when est_coup_valide conf coup ->
      let updated_cases = List.map (fun (c, col) -> if c = c1 then (c2, col) else (c, col)) cases in
      (updated_cases, couleurs)
  | _ -> conf;;
  
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

