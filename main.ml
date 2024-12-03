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

(*Q12*)


(*Q15*)

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

let () =
  print_endline "Polynôme 1 :";
  print_polynome p1;
  print_endline "Polynôme 2 :";
  print_polynome p2;

  (* Test pour Q3 *)
  let result = poly_add p1 p2 in
  print_endline "Résultat de l'addition :";
  print_polynome result;
;;

(* Resultat attendu : 
Polynôme 1 :
3x^2 + -5x^1 + 7x^0 
Polynôme 2 :
2x^3 + 5x^1 + -7x^0 
Résultat de l'addition :
2x^3 + 3x^2
*)
