(* Programming Languages, Dan Grossman *)
(* Section 1: Pairs and Tuples *)

(* pairs *)
fun swap (pr: int * bool) =
    (#2 pr, #1 pr)

(* (int * int) * (int * int) -> int *)
fun sum_pair(pr1: int * int, pr2: int * int) =
    (#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2)

(* int * int -> int *)
fun div_mod (x : int, y : int) =
    (x div y, x mod y)

fun sort_pair(pr: int * int) =
    if (#2 pr < #1 pr) 
    then (#2 pr, #1 pr)
    else pr

(* nested pairs *)

val x1 = (7,(true,9)) (* int * (bool*int) *)

val x2 = #1 (#2 x1)  (* bool *)

val x3 = (#2 x1)      (* bool*int *)

val x4 = ((3,5),((4,8),(0,0))) (* (int * int) * ((int * int) * (int * int)) *)