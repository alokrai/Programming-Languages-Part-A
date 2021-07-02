(* Programming Languages, Dan Grossman *)
(* Section 1: List Functions *)

(* Functions taking or producing lists *)
fun sum_list (lst: int list) =
    if null lst
    then 0
    else (hd lst) + sum_list (tl lst)

fun list_product (lst: int list) =
    if null lst
    then 1
    else (hd lst) * list_product (tl lst)

fun countdown (x: int) =
    if x = 0
    then []
    else x :: countdown (x - 1)

fun append (xs : int list, ys : int list) = (* part of the course logo :) *)
    if null xs
    then ys
    else hd xs :: append (tl(xs), ys)

fun sum_pair_list (xs: (int * int) list) =
    if null xs
    then 0
    else #1 (hd xs) + #2 (hd xs) + sum_pair_list (tl xs)

fun firsts (xs: (int * int) list) =
    if null xs
    then []
    else #1 (hd xs) :: firsts (tl xs)

fun seconds (xs: (int * int) list) =
    if null xs
    then []
    else #2 (hd xs) :: seconds (tl xs)

fun sum_pair_list2 (xs: (int * int) list) =
    sum_list (firsts xs) + sum_list (seconds xs)