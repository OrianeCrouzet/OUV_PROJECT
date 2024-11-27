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
      else if p1'.degree < p2'.degree then
        Some { coef = p1'.coef; degree = p1'.degree; suite = poly_add p1'.suite (Some p2') }
      else
        Some { coef = p2'.coef; degree = p2'.degree; suite = poly_add (Some p1') p2'.suite }
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

(*Q12  -  Il me faut vraiment la question 11 pour celle-là sinon je vois pas trop quoi faire :(( *)


(*Q15*)

(*Q18*)


(*TESTING*)
