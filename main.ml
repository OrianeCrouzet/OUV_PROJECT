(*Q1*)

type polynome = {
  coef : int;
  degree : int;
  suite : polynome option;
}

(*Q2*)

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


(*Q4*)


(*Q5  -  J'avais besoin de la structure de données pour visualiser la question 6, Youra tu pourras changer si tu trouves
une strucutre meilleure après, je suis pas certaine de mon truc lol *) 

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
       | _ -> Pow(base, n))  (* Traitement normal si les pow ne sont pas imbriqués *)
  | Int _ -> failwith "Exposant invalide : doit être strictement positif"
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


(*Q15*)

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
let expr_list_to_polynomial_product_naif (expr_list : expr list) : polynome option =
  let polys = List.map arb2poly expr_list in
  let rec multiply_polynomials polynomes =
    match polynomes with
    | [] -> None 
    | [p] -> p   
    | p1 :: p2 :: rest -> multiply_polynomials(poly_prod p1 p2 :: rest)
  in 
  multiply_polynomials polys
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


(*Q18*)


(*TESTING*)

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

(* Resultat attendu : 
Polynôme 1 :
3x^2 + -5x^1 + 7x^0 
Polynôme 2 :
2x^3 + 5x^1 + -7x^0 
Résultat de l'addition :
2x^3 + 3x^2
*)

(*
(* Test pour Q12 *)
let abr = list2abr [2; 1; 3; 4];;
(* Arbre de droite dans la figure 1 du sujet *)
let abr_etiquete = Node("+", Node("*", Node("123", V, V), Node("x", V, V)), Node("+", Node("42", V, V), Node("^", Node("x",V, V), Node("3", V, V))))

Printf.printf "Arbre étiqueté : ";
print_arbre_etiquete abr_etiquete;
Printf.printf "\n";

let expression = gen_arb abr_etiquete;;

(* Résultat attendu : arbre de gauche de la figure 1 du sujet
  Add[Int 42; Pow(Var "x", 3); Mul[Int 123, Pow(Var "x", 1)]]
*)
*)
;;