(** BASICS **)

(* load files into ocaml program/sessions *)
#use "hello.ml" (* loads the file hello.ml into the current program *)

(* primitive ocaml types
 *   int         31-bit signed int (roughly +/- 1 billion) on 32-bit processors, or 63-bit signed int on 64-bit processors
 *   float       IEEE double-precision floating point, equivalent to C's double
 *   bool        A boolean, written either true or false
 *   char        An 8-bit character
 *   string      A string
 *   unit        Written as ()
 *)

let x: int = 0
let x: float = 0.0
let x: bool = true
let x: char = 'h'
let x: string = "hello"
let x: unit = ()

(* implicit casting *)
(* xx_of_yy means convert from yy -> xx *)

(* note that two statements below are equivalent *)
let x = float_of_int 1 +. 1.0
let x = float 1 +. 1.0

let x = int_of_float 1.0
let x = char_of_int 1 (* equivalent to chr() in python *)
let x = int_of_char '1' (* equivalent to ord() in python *)

(* recursive functions *)

(* factorial *)
let rec fact n =
  if n <= 1 then 1
  else n * fact (n - 1)

(* fibonacci *)
let rec fib n =
  if n < 3 then 1
  else fib (n - 1) + fib (n - 2)

(* repeat *)

(* note that the 2 repeat_print functions below are equivalent *)
let rec repeat_print s n =
  if n != 0 then (
    print_string s;
    repeat_print s (n - 1)
  )

let rec repeat_print s n =
  if n != 0 then
    begin
      print_string s;
      repeat_print s (n - 1)
    end

let rec repeat s n =
  if n = 0 then ""
  else s ^ repeat s (n - 1)

(* even-odd: define mutually recursive functions *)

(* note that the two functions below are equivalent *)
let rec even n =
  match n with
    | 0 -> true
    | x -> odd (x-1)
and odd n =
  match n with
    | 0 -> false
    | x -> even (x-1)

let rec even = function
    | 0 -> true
    | x -> odd (x-1)
and odd = function
    | 0 -> false
    | x -> even (x-1)

(* type inference from function definition *)

(* note that a and b are inferred as floats *)
let avg a b =
  (a +. b) /. 2.0

(* polymorphic functions *)

(* note that x can be of any type 'a *)
let always_return_3 x = 3


(** STRUCTURE OF OCAML PROGRAMS **)

(* local variable *)

let avg a b =
  let sum = a +. b in
  sum /. 2.0

(* when local variable is actually useful *)

(* looking for (a + b) + (a + b)^2 *)
let f a b =
  (a +. b) +. (a +. b) ** 2.0

let f a b =
  let x = a +. b in
  x +. x ** 2.0

(* reference *)
let my_ref = ref 0
let x = my_ref := 100
let x = !my_ref (* dereference the reference *)

(* continue from
 * https://ocaml.org/learn/tutorials/structure_of_ocaml_programs.html*)
(* nested function *)
