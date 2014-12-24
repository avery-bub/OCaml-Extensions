
exception NotInvertible

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


(* 
  This is a function that calculates the GCD of its arguments.
  PRE: x >= y; y not equal to zero.
  POST: returns an integer that is the GCD of x and y
*)
let rec euclidean_algorithm (x : int) (y : int) : int =
  match x mod y with
    | 0 -> y
    | _ ->
      let t = x/y in 
      let r = x mod (t * y) in 
      euclidean_algorithm y r 


(* 
  This is a function that computes the modular multiplicative inverse. For example,
  if we want to find the inverse of 17 mod 24, the arguments would be:
  x = 17
  m = 24
  t0 = 0
  t1 = 1
  r0 = 24
  r1 = 17.
  Admittedly, this seems a little complicated. However:
  t0 will ALWAYS be entered as 0, t1 will ALWAYS be entered as 1, r0 will
  ALWAYS be entered as the mod m, and r1 will ALWAYS be enetered as the x value.
  So, all you really have to choose is the number being inversed (x) and its mod (m)
*)
let rec modular_multiplicative_inverse (x : int) (m : int) (t0 : int) (t1 : int) 
  (r0 : int) (r1 : int) : int =
    match r1 with
      | 0 -> if r0 > 1 then raise NotInvertible else t0 + m 
      | _ ->
        let q = r0/r1 in 
        modular_multiplicative_inverse x m t1 (t0 - (q * t1)) r1 (r0 - (q * r1))
