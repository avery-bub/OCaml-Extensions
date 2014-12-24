(* 
  This type represents the expression in the euclidean algorithm.
  For example, (2322, 654, 3, 360) would represent the expression
  2322 = 654*3 + 360.
*)
type expression = int * int * int * int

(* 
  This is a function that shown all of the steps of computing the
  greatest common denominator of x and y.
  PRE: x >= y; y is not equal to 0.
  POST: shows all of the steps of computing the GCD. returns unit.
*)
let rec euclidean_algorithm_steps (x : int) (y : int) : unit =
  match x mod y with
    | 0 -> print_endline ("The GCD is: " ^ (string_of_int y))
    | _ -> 
      let t = x/y in 
      let r = x mod (t * y) in
      print_endline ((string_of_int x) ^ " = " ^ (string_of_int y) ^
      	" * " ^ (string_of_int t) ^ " + " ^ (string_of_int r));
      euclidean_algorithm_steps y r


let rec euclidean_algorithm 