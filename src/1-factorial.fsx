let rec factorial(n) = 
    if n = 0 then 1
    else n * factorial(n-1);;

let rec factorialInPattern = function
    | 0 -> 1
    | n -> n * factorialInPattern(n-1);;