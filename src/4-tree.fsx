type BST<'T> =
    | Leaf
    | Node of 'T * BST<'T> * BST<'T>

let binaryTree = Node(5, Node(3, Leaf, Leaf), Leaf);;

let rec countNode = function
    | Leaf -> 0
    | Node(v, left, right) -> 1 + countNode left + countNode right;;

let rec treeDepth = function
    | Leaf -> 0
    | Node(v, left, right) -> 1 + max (treeDepth left) (treeDepth right);;

let rec isExists = function
    | (Leaf, e) -> false
    | (Node(v, left, right), e) -> if (e < v) then isExists(left, e) 
                                                                      elif (e > v) then isExists(right, e)
                                                                      else true;;

let rec insert = function
    | (Leaf, e) -> Node(e, Leaf, Leaf)
    | (Node(v, left, right), e) -> if (e < v) then Node(v, insert(left, e), right)
                                        elif (e > v) then Node(v, left, insert(right, e))
                                        else Node(v, left, right);;

let rec deleteSmallest = function
    | Node(v, Leaf, right) -> right
    | Node(v, left, right) -> Node(v, deleteSmallest(left), right);;