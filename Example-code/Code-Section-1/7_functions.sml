(* Programming Languages, Dan Grossman *)
(* Section 1: simple functions *)

(* this function correct only for y >= 0 *)
fun pow(x: int, y: int) =
    if y = 0
    then x
    else x * pow(x, y-1)

fun cube(x: int) = pow(x, 3)

val sixtyFour = cube(4)

val fortyTwo = pow(2, 2 + 2) + pow(4, 2) + cube(2) + 2

