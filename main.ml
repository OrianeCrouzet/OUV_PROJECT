(*Q1*)

type polynome = {
  coef : int;
  degree : int;
  suite : polynome option;
}

(*Q2*)
(*Q3*)
(*Q4*)


(*TESTING*)
let poly = {coef = 1; degree = 2; suite = Some {coef = 2; degree = 1; suite = None}}
let () = print_endline (string_of_int poly.degree)
