let rec insertion = function
    | (x, []) -> [x]
    | (x, y::ys) -> if x <= y then x::y::ys else y::insertion(x, ys);;

let rec insertionSort = function
    | [] -> []
    | x::xs -> insertion(x, insertionSort(xs));;