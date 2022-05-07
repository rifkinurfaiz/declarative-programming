let max(a, b, c) =
    let maxOfTwo(a, b) = if a > b then a else b

    maxOfTwo(maxOfTwo(a, b), c);;

let maxInPattern(a, b, c) = 
    let maxOfTwo = function
        | (a, b) -> if a > b then a else b
    
    maxOfTwo(maxOfTwo(a, b), c);;