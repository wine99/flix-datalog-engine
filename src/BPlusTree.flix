///
/// A `BPlusTree` contains: m (maxDegree) and a root `Node`
///
pub enum BPlusTree[k: Type, v: Type, r: Region](Region[r], Ref[Node[k, v, r], r])

///
/// A `Node` contains: a count (of existing keys), keys, children or values (if inner or leaf)
/// a link to the right sibling, and a link to the parent
/// TODO implementation without the parent link?
/// TODO redistribution when inserting into a full node
///
enum Node[k: Type, v: Type, r: Region] {
    case Inner(Ref[Int32, r], Array[k, r], Array[Node[k, v, r], r], Ref[Option[Node[k, v, r]], r], Ref[Option[Node[k, v, r]], r])
    case  Leaf(Ref[Int32, r], Array[k, r], Array[v, r],             Ref[Option[Node[k, v, r]], r], Ref[Option[Node[k, v, r]], r])
}

mod BPlusTree {

    ///
    /// Returns a new `m`-way B+Tree, i.e. each node has capacity `m-1` and at most `m` children.
    ///
    pub def empty(rc: Region[r], m: Int32): BPlusTree[k, v, r] \ r =
        BPlusTree(rc, Ref.fresh(rc, Node.Leaf(Ref.fresh(rc, 0), Array.empty(rc, m - 1), Array.empty(rc, m - 1), Ref.fresh(rc, None), Ref.fresh(rc, None))))

    ///
    /// Returns `true` if and only if the tree is empty.
    ///
    pub def isEmpty(t: BPlusTree[k, v, r]): Bool \ r =
        let BPlusTree(_, root) = t;
        match deref root {
            case Node.Leaf(c, _, _, _, _) => Ref.get(c) == 0
            case _ => false
        }

    ///
    /// Returns `Some(v)` if `k -> v` is in the B+Tree `t`. Otherwise returns `None`.
    ///
    pub def get(k: k, t: BPlusTree[k, v, r]): Option[v] \ r with Order[k] =
        let BPlusTree(_, root) = t;
        let leaf = getLeaf(k, deref root);
        // masked_cast(println(nodeToString(leaf)));
        let (_, v) = searchNode(k, leaf);
        v

    ///
    /// Insert the binding `k -> v` into the B+Tree `t`, replacing the existing binding.
    ///
    pub def insert(k: k, v: v, t: BPlusTree[k, v, r]): Unit \ r with Order[k], ToString[k], ToString[v] =
        let BPlusTree(rc, root) = t;
        // masked_cast(println("inserting ${k} ${v}"));
        let leaf = if (isEmpty(t)) deref root else getLeaf(k, deref root);
        // masked_cast(println(nodeToString(rc, leaf)));
        let (i, vv) = if (isEmpty(t)) (0, None) else searchNode(k, leaf);
        // masked_cast(println(i));
        match leaf {
            case Node.Inner(_, _, _, _, _) => bug!("`getLeaf` should always return a leaf node")
            case Node.Leaf(c, keys, vals, _, parent) => match vv {
                case Some(_) => Array.put(v, i, vals)
                case None(_) =>
                    match (arrayInsert!(k, i, deref c, keys), arrayInsert!(v, i, deref c, vals)) {
                        case (Some(overflowedKey), Some(overflowedVal)) =>
                            /// split the leaf
                            let (newLeaf, newKey) = splitNode!(rc, leaf, overflowedKey, Some(overflowedVal), None);
                            /// call `insertInner!` which might recurse and create a parent for the root
                            match deref parent {
                                case Some(p) => insertInner!(rc, newKey, newLeaf, p)
                                case None => bug!("`splitNode!` should have set the parents to `Some`")
                            };
                            /// if the root now has a parent, change the root
                            if (not isRoot(deref root)) match getParent(deref root) {
                                case Some(p) => root := p
                                case None => bug!("`insertInner!` should have set the parents to `Some`")
                            } else ()
                        case (None, None) =>
                            c := deref c + 1
                        case _ => bug!("`arrayInsert` should always return either both or none")
                    }
            }
        }

    ///
    /// Returns a string representation of the B+Tree.
    ///
    /// Each line represents a level of the tree, with the root at the top.
    ///
    pub def toString(t: BPlusTree[k, v, r]): String \ r with ToString[k], ToString[v] =
        let BPlusTree(rc, root) = t;
        let queue = MutDeque.empty(rc);
        let sb = StringBuilder.empty(rc);
        MutDeque.pushBack((0, deref root), queue);
        def loop(previousLevel) = match MutDeque.popFront(queue) {
            case None => ()
            case Some((level, node)) =>
                if (level > previousLevel) StringBuilder.appendString!("\n", sb) else ();
                StringBuilder.appendString!(nodeToString(rc, node), sb);
                StringBuilder.appendString!(" ", sb);
                let validChildren = validChildren(rc, node) |> Array.toVector;
                foreach (child <- validChildren) {
                    MutDeque.pushBack((level + 1, child), queue)
                };
                loop(level)
        };
        loop(0);
        StringBuilder.toString(sb)

    def nodeToString(rc: Region[r], node: Node[k, v, r]): String \ r with ToString[k], ToString[v] = match node {
        case Node.Inner(_, _, _, _, _) =>
            "[" + Array.join(" ", validKeys(rc, node)) + "]"
        case Node.Leaf(_, _, _, _, _) =>
            "[" + Array.join(" ", Array.zip(rc, validKeys(rc, node), validVals(rc, node))) + "]"
    }

    ///
    /// Search down to a leaf node where the key would be found or inserted.
    ///
    /// TODO Improve performance: if find an exact match in inner node, go to the leftmost leaf of the right child
    ///
    def getLeaf(k: k, node: Node[k, v, r]): Node[k, v, r] \ r with Order[k] = match node {
        case Node.Leaf(_, _, _, _, _) => node
        case Node.Inner(_, _, children, _, _) =>
            let (i, _) = searchNode(k, node);
            getLeaf(k, Array.get(i, children))
    }

    ///
    /// Search for a key in a node. Returns an index and the value if the node is a leaf and the key exists.
    /// 
    /// For internal nodes, return the index
    /// of the child to follow and `None`. For leaf nodes, if the key exists,
    /// return the index and the value. If the key does not exist, return the index
    /// of where the key would be inserted and `None`.
    ///
    def searchNode(k: k, node: Node[k, v, r]): (Int32, Option[v]) \ r with Order[k] = match node {
        case Node.Inner(c, keys, _, _, _) => {
            (searchInner(k, 0, deref c - 1, keys), None)
        }
        case Node.Leaf(c, keys, vals, _, _) => {
            match searchLeaf(k, 0, deref c - 1, keys) {
                case Result.Err(i) => (i, None)
                case Result.Ok(i)  => (i, Array.nth(i, vals))
            }
        }
    }

    ///
    /// Binary search for a key in an array of keys in an inner node. Returns the index of the child to follow.
    ///
    /// The search range is inclusive, and the search should always succeed. If in the initial call,
    /// `start < end` holds, `start` is returned. This handles the case where there is no key in the array,
    /// which only happens when a new root is created.
    ///
    def searchInner(k: k, start: Int32, end: Int32, keys: Array[k, r]): Int32 \ r with Order[k] =
        if (start > end)
            start
        else
            let mid = (start + end) / 2;
            let midKey = Array.get(mid, keys);
            match k <=> midKey {
                case Comparison.LessThan => if (start == mid) mid else searchInner(k, start, mid - 1, keys)
                case Comparison.EqualTo  => mid + 1
                case _                   => if (end == mid) mid + 1 else searchInner(k, mid + 1, end, keys)
            }

    ///
    /// Binary search for a key in an array of keys in a leaf node. Returns the index of the key if the key is found.
    /// Otherwise returns the index of where the key would be inserted.
    ///
    /// The search range is inclusive, and the search should always succeed. If the the key is found,
    /// `Result.Ok(index)` is returned. If the key is not found, `Result.Err(index)` is returned.
    ///
    def searchLeaf(k: k, start: Int32, end: Int32, keys: Array[k, r]): Result[Int32, Int32] \ r with Order[k] =
        if (start > end)
            bug!("`searchLeaf` should always find a index")
        else
            let mid = (start + end) / 2;
            let midKey = Array.get(mid, keys);
            match k <=> midKey {
                case Comparison.LessThan => if (start == mid) Result.Err(mid) else searchLeaf(k, start, mid - 1, keys)
                case Comparison.EqualTo  => Result.Ok(mid)
                case _                   => if (end == mid) Result.Err(mid + 1) else searchLeaf(k, mid + 1, end, keys)
            }

    ///
    /// Insert `v` at index `i` in an array `arr` where `[0, c]` are valid elements, shifting the elements to the right,
    /// possibly returning the overflowed element.
    ///
    def arrayInsert!(v: v, i: Int32, c: Int32, arr: Array[v, r]): Option[v] \ r =
        if (c == Array.length(arr) and i == c)
            Some(v)
        else
            let ret = arrayShiftRight!(i, c, arr);
            Array.put(v, i, arr);
            ret

    ///
    /// Shift the elements of an array `arr` where `[0, c]` are valid elements to the right, starting at index `i` + 1.
    /// If an element is shifted out of the array, return it.
    ///
    def arrayShiftRight!(i: Int32, c: Int32, arr: Array[v, r]): Option[v] \ r =
        def loop(ii, cc) = {
            if (ii >= cc) ()
            else
                let last = Array.get(cc - 1, arr);
                Array.put(last, cc, arr);
                loop(ii, cc - 1)
        };
        if (c == Array.length(arr)) {
            let ret = Array.nth(c - 1, arr);
            loop(i, c - 1);
            ret
        } else {
            loop(i, c);
            None
        }

    def _isFull(node: Node[k, v, r]): Bool \ r =
        match node {
            case Node.Inner(c, keys, _, _, _) => deref c == Array.length(keys)
            case Node.Leaf(c, keys, _, _, _)  => deref c == Array.length(keys)
        }

    ///
    /// Takes a full node and a pair of overflowed key-value (if the node is a leaf node)
    /// or a pair of overflowed key-child (if the node is an inner node), splits the node into two nodes,
    /// with the overflowed key-value or key-child placed in the new node, returns the new node and the key to be inserted in the parent.
    ///
    /// The splitted node and the new node will have the same parent, hence a new root will be created if necessary.
    ///
    /// TODO not so elegant to have the two optional parameters
    ///
    def splitNode!(rc: Region[r], node: Node[k, v, r], overflowedKey: k, overflowedVal: Option[v], overflowedChild: Option[Node[k, v, r]]): (Node[k, v, r], k) \ r = match node {
        case Node.Leaf(c, keys, vals, right, parent) =>
            // masked_cast(println("splitting ${nodeToString(rc, node)}"));
            let cc = deref c;
            let mid = (cc + 1) / 2;
            let newKeys = Array.empty(rc, cc);
            let newVals = Array.empty(rc, cc);
            let newRight = Ref.fresh(rc, deref right);
            let newParent = Ref.fresh(rc, deref parent);
            copyOfRange!(keys, mid, cc, newKeys, 0);
            copyOfRange!(vals, mid, cc, newVals, 0);
            Array.put(overflowedKey, cc - mid, newKeys);
            Array.put(match overflowedVal {
                case Some(v) => v
                case None    => bug!("`splitNode` should receive an overflowed value for a leaf node")
            }, cc - mid, newVals);
            let newNode = Node.Leaf(Ref.fresh(rc, cc - mid + 1), newKeys, newVals, newRight, newParent);
            c := mid;
            right := Some(newNode);
            match deref parent {
                case None =>
                    let newRootChildren = Array.empty(rc, cc + 1);
                    Array.put(node, 0, newRootChildren);
                    let newRoot = Node.Inner(Ref.fresh(rc, 0), Array.empty(rc, cc), newRootChildren, Ref.fresh(rc, None), Ref.fresh(rc, None));
                    parent := Some(newRoot);
                    newParent := Some(newRoot)
                case Some(_p) =>
                    // masked_cast(println("parent: ${nodeToString(rc, p)}"));
                    ()
            };
            (newNode, Array.get(0, newKeys))
        case Node.Inner(c, keys, children, right, parent) =>
            // masked_cast(println("splitting ${nodeToString(rc, node)}"));
            let cc = deref c;
            let mid = (cc + 1) / 2;
            let newKeys = Array.empty(rc, cc);
            let newChildren = Array.empty(rc, cc + 1);
            let newRight = Ref.fresh(rc, deref right);
            let newParent = Ref.fresh(rc, deref parent);
            copyOfRange!(keys, mid + 1, cc, newKeys, 0);
            copyOfRange!(children, mid + 1, cc + 1, newChildren, 0);
            Array.put(overflowedKey, cc - mid - 1, newKeys);
            Array.put(match overflowedChild {
                case Some(child) => child
                case None    => bug!("`splitNode` should receive an overflowed child for an inner node")
            }, cc - mid, newChildren);
            let newNode = Node.Inner(Ref.fresh(rc, cc - mid), newKeys, newChildren, newRight, newParent);
            foreach (child <- validChildren(rc, newNode) |> Array.toVector) {
                setParent(child, Some(newNode))
            };
            c := mid;
            right := Some(newNode);
            match deref parent {
                case None =>
                    let newRootChildren = Array.empty(rc, cc + 1);
                    Array.put(node, 0, newRootChildren);
                    let newRoot = Node.Inner(Ref.fresh(rc, 0), Array.empty(rc, cc), newRootChildren, Ref.fresh(rc, None), Ref.fresh(rc, None));
                    parent := Some(newRoot);
                    newParent := Some(newRoot)
                case Some(_p) =>
                    // masked_cast(println("parent: ${nodeToString(rc, p)}"));
                    ()
            };
            (newNode, Array.get(mid, keys))
    }

    ///
    /// Copy the elements of array `src` from index `start`(inclusive) to `end`(exclusive) to an array `dest` starting at index `destStart`.
    ///
    def copyOfRange!(src: Array[v, r], start: Int32, end: Int32, dest: Array[v, r], destStart: Int32): Unit \ r =
        if (start >= end) ()
        else
            let v = Array.get(start, src);
            Array.put(v, destStart, dest);
            copyOfRange!(src, start + 1, end, dest, destStart + 1)

    ///
    /// Insert a key `k` and a node `n` into an inner node `node`.
    ///
    /// The operation might recurse if `node` overflows. If the root overflows, a new root is created.
    ///
    def insertInner!(rc: Region[r], k: k, n: Node[k, v, r], node: Node[k, v, r]): Unit \ r with Order[k] = match node {
        case Node.Inner(c, keys, children, _, parent) =>
            let (i, _) = searchNode(k, node);
            match (arrayInsert!(k, i, deref c, keys), arrayInsert!(n, i + 1, deref c + 1, children)) {
                case (Some(overflowedKey), Some(overflowedChild)) =>
                let (newNode, newKey) = splitNode!(rc, node, overflowedKey, None, Some(overflowedChild));
                match deref parent {
                    case Some(p) => insertInner!(rc, newKey, newNode, p)
                    case None => bug!("`splitNode!` should have set the parents to `Some`")
                }
                case (None, None) =>
                    c := deref c + 1
                case _ => bug!("`arrayInsert` should always return either both or none")
            }
        case Node.Leaf(_, _, _, _, _) => bug!("`insertInner!` should always receive an inner node")
    }

    def isRoot(node: Node[k, v, r]): Bool \ r =
        match node {
            case Node.Inner(_, _, _, _, parent) => match deref parent {
                case None => true
                case _    => false
            }
            case Node.Leaf(_, _, _, _, parent) => match deref parent {
                case None => true
                case _    => false
            }
        }

    def getParent(node: Node[k, v, r]): Option[Node[k, v, r]] \ r =
        match node {
            case Node.Inner(_, _, _, _, parent) => deref parent
            case Node.Leaf(_, _, _, _, parent)  => deref parent
        }

    def setParent(node: Node[k, v, r], parent: Option[Node[k, v, r]]): Unit \ r =
        match node {
            case Node.Inner(_, _, _, _, p) => p := parent
            case Node.Leaf(_, _, _, _, p)  => p := parent
        }

    def validKeys(rc: Region[r], node: Node[k, v, r]): Array[k, r] \ r = match node {
        case Node.Inner(c, keys, _, _, _) => Array.slice(rc, start = 0, end = deref c, keys)
        case Node.Leaf(c, keys, _, _, _)  => Array.slice(rc, start = 0, end = deref c, keys)
    }

    def validVals(rc: Region[r], node: Node[k, v, r]): Array[v, r] \ r = match node {
        case Node.Leaf(c, _, vals, _, _) => Array.slice(rc, start = 0, end = deref c, vals)
        case Node.Inner(_, _, _, _, _)   => Array.empty(rc, 0)
    }

    def validChildren(rc: Region[r], node: Node[k, v, r]): Array[Node[k, v, r], r] \ r = match node {
        case Node.Inner(c, _, children, _, _) => Array.slice(rc, start = 0, end = deref c + 1, children)
        case Node.Leaf(_, _, _, _, _)         => Array.empty(rc, 0)
    }

    @Test
    pub def testSplit01(): Bool \ IO = region rc {
        let t = BPlusTree.empty(rc, 5);
        BPlusTree.insert(20, 20, t);
        BPlusTree.insert(10, 10, t);
        BPlusTree.insert(30, 30, t);
        BPlusTree.insert(25, 25, t);
        let BPlusTree.BPlusTree(_, root) = t;
        let (newNode, newKey) = BPlusTree.splitNode!(rc, deref root, 35, Some(35), None);
        println(nodeToString(rc, deref root));
        println(nodeToString(rc, newNode));
        println(newKey);
        match deref root {
            case Node.Leaf(_, _, _, _, parent) =>
                match deref parent {
                    case Some(Node.Inner(c, _, _, _, _)) =>
                        println(deref c);
                        true
                    case _ => false
                }
            case _ => false
        }
    }

}
