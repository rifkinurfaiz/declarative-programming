type Tree<'T> =
    | Leaf
    | Node of 'T * Tree<'T> * Tree<'T>

let binaryTree = Node(5, Node(3, Leaf, Leaf), Leaf);;

let rec countNode = function
    | Leaf -> 0
    | Node(v, branch1, branch2) -> 1 + countNode branch1 + countNode branch2;;

let rec treeDepth = function
    | Leaf -> 0
    | Node(v, branch1, branch2) -> 1 + max (treeDepth branch1) (treeDepth branch2);;

let rec isExists = function
    | (Leaf, e) -> false
    | (Node(v, branch1, branch2), e) -> if (e < v) then isExists(branch1, e) 
                                                                      elif (e > v) then isExists(branch2, e)
                                                                      else true;;

let rec insert = function
    | (Leaf, e) -> Node(e, Leaf, Leaf)
    | (Node(v, branch1, branch2), e) -> if (e < v) then Node(v, insert(branch1, e), branch2)
                                        elif (e > v) then Node(v, branch1, insert(branch2, e))
                                        else Node(v, branch1, branch2);;