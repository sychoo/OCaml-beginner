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
let rec repeat str n: int =
  if n != 0 then
    begin
      print_string str
      repeat (str, n - 1)
    end

(* even-odd *)

(* type inference from function definition *)

(* note that a and b are inferred as floats *)
let avg a b =
  (a +. b) /. 2.0

(* polymorphic functions *)

(* note that x can be of any type 'a *)
let always_return_3 x = 3
