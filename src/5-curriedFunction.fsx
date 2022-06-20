let prefix = (fun (job: string) -> (fun name -> job + name));;

let promote = prefix "Professor ";;

promote "Snape";;

(prefix "Doctor ") "Strange";;

let rec sum f = function 
    | 0 -> 0
    | m -> f(m) + sum f (m-1);;

sum (fun k -> (k * k)) 5;;