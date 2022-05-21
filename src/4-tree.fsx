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
