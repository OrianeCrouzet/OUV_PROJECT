(*Q1*)

type polynome = {
  coef : int;
  degree : int;
  suite : polynome option;
}

(*Q2*)

let rec canonique (p: polynome option) : polynome option =
  (* on convertit un polynome chaîné en une liste de tuples (coefficient, degré) *)
  let rec to_list p =
    match p with
    | None -> []
    | Some { coef; degree; suite } -> (coef, degree) :: (to_list suite)
  in

  (* on convertit une liste de tuples (coefficient, degré) en un polynome chaîné *)
  let rec from_list lst =
    match lst with
    | [] -> None
    | (coef, degree) :: rest -> Some { coef; degree; suite = from_list rest }
  in

  (* on trie et combine les monômes *)
  let sorted_p = List.sort (fun (_, d1) (_, d2) -> compare d1 d2) (to_list p) in

  let rec combine p =
    match p with
    | [] -> []
    | (c1, d1) :: (c2, d2) :: q when d1 = d2 -> combine ((c1 + c2, d1) :: q)
    | (c, d) :: q when c = 0 -> combine q  (* on enleve les monômes de coefficient nul *)
    | (c, d) :: q -> (c, d) :: (combine q)
  in

  (* et puis ici on convertit la liste combinée et triée en un polynome chaîné *)
  from_list (combine sorted_p)

(*Le tri a une complexité de O(n log n), où n est le nombre de monômes dans le polynôme. La fonction aux a une complexité de O(n) où n est le nombre de monômes dans le polynôme. La complexité de la fonction canonique est donc de O(n log n) avec le tri *)


(*Q3*)

let rec poly_add p1 p2 =
  match (p1, p2) with
  | (None, p) | (p, None) -> p  (* Si l'un des polynômes est vide, retourner l'autre *)
  | (Some p1', Some p2') ->
      if p1'.degree = p2'.degree then
        let coef_sum = p1'.coef + p2'.coef in
        if coef_sum = 0 then
          poly_add p1'.suite p2'.suite  (* On ignore les termes dont le coefficient est 0 *)
        else
          Some { coef = coef_sum; degree = p1'.degree; suite = poly_add p1'.suite p2'.suite }
      else if p1'.degree > p2'.degree then
        Some { coef = p1'.coef; degree = p1'.degree; suite = poly_add p1'.suite (Some p2') }
      else
        Some { coef = p2'.coef; degree = p2'.degree; suite = poly_add (Some p1') p2'.suite }
;;

(*Q4*)

let poly_prod poly1 poly2 = 
  let rec multibyterm poly1 terme =
    match poly1, terme with
    | _, None -> None
    | None, _ -> None
    | Some { coef = c1; degree = d1; suite = s1 }, Some { coef = c2; degree = d2; _ } ->
        let new_term = { coef = c1 * c2; degree = d1 + d2; suite = None } in
        let res_suite = multibyterm s1 terme in
        poly_add (Some new_term) res_suite
  in

  let rec mulandadd poly1 poly2 =
    match poly1 with 
    | None -> None
    | Some { coef = c1; degree = d1; suite = s1 } -> 
        let tmp = Some { coef = c1; degree = d1; suite = None } in
        let res = multibyterm poly2 tmp in
        poly_add res (mulandadd s1 poly2)
  in
  mulandadd poly1 poly2 


(*Q5*) 

type expr =
  | Int of int
  | Var of string
  | Pow of expr * int
  | Add of expr list
  | Mul of expr list
;;

(*Q6*)

let arbre_gauche = 
  Add [
    Mul [Int 123; Pow (Var "x", 1)];
    Int 42;
    Pow (Var "x", 3)
  ]
;;

(*Q7*)

let rec arb2poly (e: expr) : polynome option =
  let rec aux e =
    match e with
    | Int c -> Some { coef = c; degree = 0; suite = None }
    | Var "x" -> Some { coef = 1; degree = 1; suite = None }
    | Pow (Var "x", d) -> Some { coef = 1; degree = d; suite = None }
    | Add l -> List.fold_left (fun acc e -> poly_add acc (aux e)) None l
    | Mul l -> List.fold_left (fun acc e -> poly_prod acc (aux e)) (Some { coef = 1; degree = 0; suite = None }) l
  in
  aux e
;;

(*Q8*)

let extraction_alea l p =
  (* on initialise un générateur aléatoire *)
  Random.self_init ();

  let n = List.length l in
  if n = 0 then ([], p)  (* si la liste L est vide, on retourne L et P sans changement *)
  else
    let r = Random.int n in (* on choisi un entier aléatoire entre 0 et n-1 *)
    
    Printf.printf "r = %d\n" r;

    let rec aux l p i =
      match l with
      | [] -> ([], p)  (* Si L est vide, on retourne deux listes inchangées *)
      | h :: t ->
          if i = r then
            (t, h :: p)  (* On retire le r-ième élément de L et on l'ajoute en tête de P *)
          else
            let (l2, p2) = aux t p (i + 1) in
            (h :: l2, p2)  (* Sinon, on continue à parcourir la liste *)
    in
    aux l p 0
;;

(*Q9*)

let gen_permutation n =
  let rec aux l p =
    if l = [] then p  (* On s'arrête quand L est vide *)
    else
      let (l2, p2) = extraction_alea l p in
      aux l2 p2
  in
  aux (List.init n (fun i -> i + 1)) []  (* On initialise L avec les entiers 1 à n grâce à List.init, et P comme liste vide *)
;;

(*Q10*)

type arbre_binaire =
  | Empty
  | Node of int * arbre_binaire * arbre_binaire

let list2abr list =
  let rec elemInAbr abr elem =
    match abr with
    |Empty -> Node(elem, Empty, Empty)
    |Node(e, g, d) -> 
        if elem < e then
          Node(e, elemInAbr g elem, d)
        else if elem > e then
          Node(e, g, elemInAbr d elem)
        else
          abr
  in
  
  let rec list2abr_aux list abr = 
    match list with
    |[] -> abr
    |l::ist -> list2abr_aux ist (elemInAbr abr l)
  in
  list2abr_aux list Empty
  
  (*Q10bis -> fonction pour test*)
let minAbr =
  let rec minAbr_aux abr =
    match abr with
    |Empty -> failwith "Empty tree"
    |Node(e, Empty, _) -> e
    |Node(_, g, _) -> minAbr_aux g
  in
  minAbr_aux
  
let printAbr =
  let rec printAbr_aux abr =
    match abr with
    |Empty -> ()
    |Node(e, g, d) -> 
        printAbr_aux g;
        print_int e;
        printAbr_aux d
  in
  printAbr_aux
;;

(*Q11*)
(* Type de l'arbre étiqueté *)
type arbre_etiquete =
  | V
  | Node of string * arbre_etiquete * arbre_etiquete


 (* Fonction pour étiqueter un sous-nœud vide *)
let etiquetage_feuille () =
  if Random.bool () then
    Node(string_of_int (Random.int 101), V, V)
  else
    Node("x", V, V)


(* Fonction d'étiquetage *)
let rec etiquetage a =
  match a with
  | Empty -> V
  (* un nœud interne de valeur ℓ a deux enfants qui sont des feuilles *)
  | Node (e, Empty, Empty) ->
      if e mod 2 = 0 then
        (* Si pair a sa gauche on a x et a sa droite un entier aléatoire entre 0 et 100 *)
        Node("^", Node("x", V, V), Node(string_of_int (Random.int 101), V, V))
      else
        (* Si impair a sa gauche on a un entier aléatoire entre 0 et 100 et a sa droite x *)
        Node("*", Node(string_of_int (Random.int 401 - 200), V, V), Node("x", V, V))
  (* un nœud interne de valeur ℓ a deux enfants dont au moins un n’est pas une feuille *)
  | Node (e, g, d) ->
      let left = if g = Empty then etiquetage_feuille () else etiquetage g in
      let right = if d = Empty then etiquetage_feuille () else etiquetage d in
      let probability = Random.float 1.0 in
      if probability < 0.75 then
        (* Probabilité 3/4 pour '+' *)
        Node("+", left, right)
      else
        (* Probabilité 1/4 pour '*' *)
        Node("*", left, right)
;;

(* Fonctions utilitaires pour la Q12 *)
(* Permettent de vérifier les conditions de la grammaire 2 *)
let verif_add left right =
  match (left, right) with
  | (Add l1, Add l2) -> Add (l1 @ l2)  (* On fusionne deux additions *)
  | (Add l, e) | (e, Add l) -> Add (l @ [e])  (* On fusionne une addition avec une autre expression *)
  | (v, w) -> Add [v; w]  (* Cas général : on crée une addition avec les deux expressions *)
;;

let verif_mul left right = 
  match (left, right) with
  | (Mul l1, Mul l2) -> Mul(l1 @ l2)
  | (Mul l, e) | (e, Mul l) -> Mul(l @ [e])
  | (v, w) -> Mul [v; w]
;;

let verif_pow base exp =
  match exp with 
  | Int n when n > 0 ->  (* Si l'exposant est un entier strictement positif *)
      (match base with
       | Pow(Var"x", 1) -> Pow(Var "x", n)  (* Si on lit un Pow(Var "x", 1), on va l'ignorer et mettre la bonne puissance -> permet d'éviter les pow imbriqués inutiles *)
       | Int b when b = 0 -> Int b
       | _ -> Pow(base, n))  (* Traitement normal si les pow ne sont pas imbriqués *)
  | Int n when n = 0 -> 
      (match base with
       | Int b when b = 0 -> failwith "0^0 est strictement interdit"
       | _ -> Int 1)
  | Int _ -> failwith "Exposant invalide : doit être un entier positif"
  | _ -> failwith "Exposant invalide : doit être un entier" 
;;

(*Q12*)
let rec gen_arb a =
  match a with
  | V -> failwith "Arbre vide, pas d'expression à générer"
  | Node (v, g, d) ->
      (* Cas des opérateurs *)
      if v = "+" then
        let left = gen_arb g in  (* On transforme g et d en sous-arbre avant de les envoyer a verif_add *)
        let right = gen_arb d in
        verif_add left right
      else if v = "*" then
        let left = gen_arb g in  (* On transforme g et d en sous-arbre avant de les envoyer a verif_mul *)
        let right = gen_arb d in
        verif_mul left right
      else if v = "^" then
        let base = gen_arb g in  (* On transforme g et d en sous-arbre avant de les envoyer a verif_pow *)
        let exponent = gen_arb d in
        verif_pow base exponent
      else if v = "x" then
        (* Cas de x : on assure de rajouter une puissance de 1 à tous les x (redondances gérées dans verif_pow) *)
        Pow(Var "x", 1)
      else
        (* Cas d'un entier *)
        Int (int_of_string v)
;;

(*Q13*)

let gen_random_list n max =
  let rec aux n acc =
    if n <= 0 then acc
    else aux (n - 1) ((Random.int max) :: acc)
  in
  aux n []

let () = Random.self_init ()

let generation_exp n =
  let rec aux n acc =
    if n <= 0 then acc
    else
      (*20 la taille de la liste, les chiffres générés entre 0 et 100*)
      let liste = gen_random_list 20 100 in
      let abr_temp = list2abr liste in
      let abr_temp_etiq = etiquetage abr_temp in
      let res = gen_arb abr_temp_etiq in
      aux (n - 1) (res :: acc)
  in
  aux n []
;;

(*Q14 et Q17*)

let rec split_at n l =
  if n = 0 then
    ([], l)  
  else
    match l with
    | [] -> ([], []) 
    | head :: tail ->
        let (left, right) = split_at (n - 1) tail in 
        (head :: left, right)  
;;

(* Fonction pour transformer une liste d'expressions en polynômes et calculer leur produit : version naive *)
let expr_list_to_polynomial_sum_naif (expr_list : expr list) : polynome option =
  let polys = List.map arb2poly expr_list in
  let rec sum_polynomials polynomes =
    match polynomes with
    | [] -> None 
    | [p] -> p   
    | p1 :: p2 :: rest -> sum_polynomials(poly_add p1 p2 :: rest)
  in 
  sum_polynomials polys;
;; 

(* Fonction pour transformer une liste d'expressions en polynômes et calculer leur produit : version diviser pour régner *)
let expr_list_to_polynomial_sum_divide (expr_list : expr list) : polynome option =
  let polys = List.map arb2poly expr_list in
  let rec sum_polynomials_rec polynomes =
    match polynomes with
    | [] -> Some {coef = 1; degree = 0; suite = None}  
    | [p] -> p 
    | _ ->
        let n = List.length polynomes / 2 in
        let left, right = split_at n polynomes in
        let left_product = sum_polynomials_rec left in
        let right_product = sum_polynomials_rec right in
        match left_product, right_product with
        | lp, rp -> poly_add lp rp 
  in
  sum_polynomials_rec polys
;;

(*Q15 et Q18*) 

(* Fonction pour transformer une liste d'expressions en polynômes et calculer leur produit : version naive *)
let expr_list_to_polynomial_product_naif (expr_list : expr list) : polynome option =
  let polys = List.map arb2poly expr_list in
  let rec multiply_polynomials polynomes =
    match polynomes with
    | [] -> None 
    | [p] -> p   
    | p1 :: p2 :: rest -> multiply_polynomials(poly_prod p1 p2 :: rest)
  in 
  multiply_polynomials polys;
;; 

(* Fonction pour transformer une liste d'expressions en polynômes et calculer leur produit : version diviser pour régner *)
let expr_list_to_polynomial_product_divide (expr_list : expr list) : polynome option =
  let polys = List.map arb2poly expr_list in
  let rec multiply_polynomials_rec polynomes =
    match polynomes with
    | [] -> Some {coef = 1; degree = 0; suite = None}  
    | [p] -> p 
    | _ ->
        let n = List.length polynomes / 2 in
        let left, right = split_at n polynomes in
        let left_product = multiply_polynomials_rec left in
        let right_product = multiply_polynomials_rec right in
        match left_product, right_product with
        | lp, rp -> poly_prod lp rp 
  in
  multiply_polynomials_rec polys
;;

(*Q16*)

let generation_exponentielle_exp =
  let rec aux n acc =
    if n >= 15 then acc
    else if n = 0 then
      let abr_temp = list2abr [1] in
      let abr_temp_etiq = etiquetage abr_temp in
      let res = gen_arb abr_temp_etiq in
      aux (n + 1) (res :: acc)
    else
      let rec aux2 n list =
        match n with 
        | 0. -> list
        | _ -> aux2 (n -. 1.) ((int_of_float n) :: list)
      in
      let listz = aux2 (2. ** (float_of_int n)) [] in
      let abr_temp = list2abr listz in
      let abr_temp_etiq = etiquetage abr_temp in
      let res = gen_arb abr_temp_etiq in
      aux (n+1) (res :: acc)
  in
  aux 0 []
;;

(* FONCTIONS UTILITAIRES *)

(* Fonction auxiliaire pour afficher un polynôme *)
let rec print_polynome poly =
  match poly with
  | None -> print_endline "0"
  | Some { coef; degree; suite } ->
      (* Si le coefficient est 0, on passe au suivant *)
      if coef <> 0 then
        begin
          Printf.printf "%dx^%d " coef degree;
          (* Si la suite existe, on ajoute le signe "+" et on affiche la suite *)
          match suite with
          | None -> print_newline ()
          | Some _ -> print_string "+ "; print_polynome suite
        end
      else
        (* Si le coefficient est 0, on passe à la suite *)
        print_polynome suite
;;

(* Convertit une liste de tuples (coef, degree) en un polynome de type option *)
let rec list_to_polynome lst =
  match lst with
  | [] -> None
  | (coef, degree) :: rest -> Some { coef; degree; suite = list_to_polynome rest }
;;

(* Fonction pour afficher l'arbre étiqueté *)
let rec print_arbre_etiquete a =
  match a with
  | V -> Printf.printf "V"
  | Node (v, g, d) ->
      Printf.printf "(Node(%s) " v;
      print_arbre_etiquete g;
      Printf.printf " ";
      print_arbre_etiquete d;
      Printf.printf ")"
;; 

(* Fonction pour afficher une expression *)
let rec print_expr expr =
  match expr with
  | Int n -> Printf.printf "%d" n
  | Var x -> Printf.printf "%s" x
  | Pow (base, exp) ->
      Printf.printf "(";
      print_expr base;
      Printf.printf " ^ %d)" exp
  | Add exprs ->
      Printf.printf "(";
      List.iteri (fun i e -> if i > 0 then Printf.printf " + "; print_expr e) exprs; (* si i > 0, on affiche un +  car on met pas de + avant le premier élément *)
      Printf.printf ")"
  | Mul exprs ->
      Printf.printf "(";
      List.iteri (fun i e -> if i > 0 then Printf.printf " * "; print_expr e) exprs; (* si i > 0, on affiche un *  car on met pas de * avant le premier élément *)
      Printf.printf ")"
;;

 (* Fonction pour afficher les ABR et leurs transformations *)
let display_transformed_abrs abr_list =
  List.iteri
    (fun i expr ->
       Printf.printf "=== ABR %d ===\n" (i + 1);
       Printf.printf "Expression générée :\n";
       print_expr expr; (* Affiche l'expression générée *)
       Printf.printf "\n\n")
    abr_list
;;

(* Fonction pour calculer le temps d'exécution d'une fonction : version 1 argument *)
let time_execution f arg =
  let start_time = Sys.time () in
  let result = f arg in
  let end_time = Sys.time () in
  let elapsed_time = end_time -. start_time in
  (result, elapsed_time)
;;

(*TESTING*)
(*
let p1 = Some {
    coef = 3; degree = 2; suite = Some {
        coef = -5; degree = 1; suite = Some {
            coef = 7; degree = 0; suite = None
          }
      }
  };;

let p2 = Some {
    coef = 2; degree = 3; suite = Some {
        coef = 5; degree = 1; suite = Some {
            coef = -7; degree = 0; suite = None
          }
      }
  };;

print_endline "Polynôme 1 :";
print_polynome p1;
print_endline "Polynôme 2 :";
print_polynome p2;

(* Test pour Q3 *)
let result = poly_add p1 p2 in
print_endline "Résultat de l'addition :";
print_polynome result;
*) 


(* Génération de 10 arbres binaires de recherche *)
let n = 100 in
let (abr_list, time) = time_execution generation_exp n in
Printf.printf "Affichage des %d ABR générés et transformés :\n" n;
display_transformed_abrs abr_list;
Printf.printf "\nTemps d'exécution : %.6f secondes\n" time;

(* Exécution et mesure du temps pour la méthode naive *)
let (res1, time1) = time_execution expr_list_to_polynomial_sum_naif abr_list in

(* Exécution et mesure du temps pour la méthode diviser pour régner *)
let (res2, time2) = time_execution expr_list_to_polynomial_sum_divide abr_list in

(* Affichage des résultats *)
Printf.printf "Méthode naive :\n";
print_polynome res1;
Printf.printf "\nTemps d'exécution : %.6f secondes\n" time1;

Printf.printf "Méthode diviser pour régner :\n";
print_polynome res2;
Printf.printf "\nTemps d'exécution : %.6f secondes\n" time2;

;;