// :: called cons operator which takes first index (head) of a list
let rec listLength1 = function
    | [] -> 0
    | x::xs -> 1 + listLength1(xs);;

let rec listLength2 = function
    | (n, []) -> n
    | (n, x::xs) -> listLength2(n+1, xs);;

let rec appendList = function
    | ([], ys) -> ys
    | (x::xs, ys) -> x :: appendList(xs, ys);;

let rec reverseList1 = function
    | [] -> []
    | x::xs -> reverseList1(xs) @ [x];;

let rec reverseList2 = function
    | ([], ys) -> ys
    | (x::xs, ys) -> reverseList2(xs, x::ys);;

let rec dropFirstNList = function
    | (xs, 0) -> xs
    | (x::xs, n) -> dropFirstNList(xs, n-1);;

let rec takeFirstNList = function
    | (xs, 0) -> []
    | (x::xs, n) -> x :: takeFirstNList(xs, n-1);;