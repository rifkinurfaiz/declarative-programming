let rec map f = function
    | [] -> []
    | x::xs -> f(x) :: map f xs;;

map (fun s -> s + "ppy") ["ha"; "hi"];;

let getHead (x::_) = x;;
let getTail (x::xs) = xs;;

let rec matrixTranspose = function
    | ([]::_) -> []
    | rows -> (map getHead rows) :: (matrixTranspose (map getTail rows));;

let rec dotProduct a b =
  match (a, b) with
    | ([], []) -> 0
    | (x::xs, y::ys) -> x * y + dotProduct xs ys;;

let rec foldl f = function
    | (e, []) -> e
    | (e, x::xs) -> foldl f (f(e, x), xs);;

let rec foldr f = function
    | ([], e) -> e
    | (x::xs, e) -> f(x, foldr f (xs, e));;