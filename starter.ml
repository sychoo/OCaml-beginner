(* Written by Simon Chu. *)

(* To run the program, enter ocaml <source-file>
 * To compile the program, enter ocaml -o <output-file> <source-file>,
 * then ./<output-file>
 * To load the program, enter #use <source-file>.
 *)

(* https://ocaml.org/learn/taste.html *)

(* recursive factorial function *)
let rec fact x =
  if x <= 1 then 1 else x * fact (x - 1);;

fact 10;; (* 3628800 *)


(* iterative factorial function *)
let fact n =
    let result = ref 1 in
    for i = 2 to n do
        result := i * !result
    done;
    !result;;

fact 10;; (* 3628800 *)


(* sum *)
let rec sum = function
    | [] -> 0
    | hd :: tl -> hd + sum tl;;

sum [1; 2; 3; 4; 5];; (* 15 *)


(* remove sequential duplicates *)
let rec destutter = function
    | [] -> []
    | [hd] -> [hd]
    | hd1 :: hd2 :: tl ->
        if hd1 = hd2 then destutter (hd2 :: tl) else hd1 :: destutter (hd2 :: tl);;

destutter [1; 1; 1; 2];;


(* polymorphism insertion sort *)
let rec sort = function
    | [] -> []
    | x :: l -> insert x (sort l)
and insert elem = function
    | [] -> [elem]
    | x :: l -> if elem < x then elem :: x :: l
                else x :: insert elem l;;

sort (3 :: 1 :: 2 :: []);;
sort [3; 1; 2;];;
sort ["c"; "a"; "b"];;


(* tuple *)
("I", "Love", "CMU", 2019);; (* tuple doesn't have to be homogeneous *)
let max (r1, r2): float = if r1 > r2 then r1 else r2;;
max (1.0, 1.5);; (* 1.5 *)

(* pattern matching *)
let a_tuple = (3, "three");;
let (x, y) = a_tuple;;
x;;
y;;


(* List *)
let languageList = ["OCaml"; "Perl"; "C"];;
List.length languageList;; (* find length of language list *)
List.map languageList ~f:String.length;; (* find length of each string *)
List.map ~f:String.length languageList;; (* equivalent to the expression above *)


(* records datatype *)
type point = { x: float; y: float };; (* define point record *)
type circle = { center: point; radius: float }
let magnitude { x; y } = sqrt( x ** 2. +. y ** 2. );; (* calculate the magnitude of the 2d coordinate *)
let distance p1 p2 = magnitude { x = p1.x -. p2.x; y = p1.y -. p2.y };;
magnitude { x = 3.; y = 4. };; (* 5 *)
distance { x = 3.; y = 4.} { x = 0.; y = 0. };; (* 5 *)


(* mutable records *)
type point = {
  mutable x: float;
  mutable y: float;
}
let p = { x = 1.0; y = 2.0 };;
p.x = 2.0;;
p.y = 1.0;;
p;;

(* arrays *)
let numbers = [| 1; 2; 3; 4 |];;
numbers.(2) <- 4;; (* update value *)
numbers;;

