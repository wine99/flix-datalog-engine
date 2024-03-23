///
/// A `BPlusTree` contains: m (maxDegree) and a root `Node`
///
pub enum BPlusTree[k: Type, v: Type, r: Region](Region[r], Ref[Node[k, v, r], r])

///
/// A `Node` contains: a count (of existing keys), keys, children or values (if inner or leaf)
/// a link to the right sibling, and a link to the parent
/// TODO implementation without the parent link? right-sibling to support deletion?
/// TODO redistribution when inserting into a full node
///
enum Node[k: Type, v: Type, r: Region] {
    case Inner(Ref[Int32, r], Array[k, r], Array[Node[k, v, r], r], Ref[Option[Node[k, v, r]], r], Ref[Option[Node[k, v, r]], r])
    case  Leaf(Ref[Int32, r], Array[k, r], Array[v, r],             Ref[Option[Node[k, v, r]], r], Ref[Option[Node[k, v, r]], r])
}

mod BPlusTree {
    pub def empty(rc: Region[r], m: Int32): BPlusTree[k, v, r] \ r =
        BPlusTree(rc, Ref.fresh(rc, Node.Leaf(Ref.fresh(rc, 0), Array.empty(rc, m - 1), Array.empty(rc, m - 1), Ref.fresh(rc, None), Ref.fresh(rc, None))))

    pub def isEmpty(t: BPlusTree[k, v, r]): Bool \ r =
        let BPlusTree(_, root) = t;
        match deref root {
            case Node.Leaf(c, _, _, _, _) => Ref.get(c) == 0
            case _ => false
        }

    pub def get(k: k, t: BPlusTree[k, v, r]): Option[v] \ r with Order[k], ToString[k], ToString[v] =
        let BPlusTree(_, root) = t;
        let leaf = getLeaf(k, deref root);
        // masked_cast(println(nodeToString(leaf)));
        let (_, v) = searchNode(k, leaf);
        v

    pub def insert(k: k, v: v, t: BPlusTree[k, v, r]): Unit \ r with Order[k], ToString[k], ToString[v] =
        let BPlusTree(rc, root) = t;
        // masked_cast(println("inserting ${k} ${v}"));
        let leaf = if (isEmpty(t)) deref root else getLeaf(k, deref root);
        // masked_cast(println(nodeToString(leaf)));
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

    pub def toString(t: BPlusTree[k, v, r]): String \ r with ToString[k], ToString[v] = region rc {
        let BPlusTree(_, root) = t;
        let queue = MutDeque.empty(rc);
        let sb = StringBuilder.empty(rc);
        MutDeque.pushBack((0, deref root), queue);
        def loop(previousLevel) = match MutDeque.popFront(queue) {
            case None => ()
            case Some((level, node)) =>
                if (level > previousLevel) StringBuilder.appendString!("\n", sb) else ();
                StringBuilder.appendString!(nodeToString(node), sb);
                StringBuilder.appendString!(" ", sb);
                match node {
                    case Node.Inner(c, _, children, _, _) =>
                        let validChildren = Array.toVector(children) |> Vector.slice(start = 0, end = deref c + 1);
                        foreach(child <- validChildren) {
                            MutDeque.pushBack((level + 1, child), queue)
                        }
                    case Node.Leaf(_, _, _, _, _) =>
                        ()
                };
                loop(level)
        };
        loop(0);
        StringBuilder.toString(sb)
    }

    def nodeToString[k: Type, v: Type, r: Region](node: Node[k, v, r]): String \ r with ToString[k], ToString[v] = match node {
        case Node.Inner(c, keys, _, _, _) => region rc {
            let validKeys = Array.slice(rc, start = 0, end = deref c, keys);
            "[" + Array.join(" ", validKeys) + "]"
        }
        case Node.Leaf(c, keys, vals, _, _) => region rc {
            let validKeys = Array.slice(rc, start = 0, end = deref c, keys);
            let validVals = Array.slice(rc, start = 0, end = deref c, vals);
            "[" + Array.join(" ", Array.zip(rc, validKeys, validVals)) + "]"
        }
    }

    ///
    /// Search down to a leaf node where the key would be found or inserted.
    /// TODO Improve performance: if find an exact match in inner node, go to the leftmost leaf of the right child
    ///
    def getLeaf(k: k, node: Node[k, v, r]): Node[k, v, r] \ r with Order[k] = match node {
        case Node.Leaf(_, _, _, _, _) => node
        case Node.Inner(_, _, children, _, _) =>
            let (i, _) = searchNode(k, node);
            getLeaf(k, Array.get(i, children))
    }

    ///
    /// Search for a key in a node. For internal nodes, return the index
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

    def searchLeaf(k: k, start: Int32, end: Int32, keys: Array[k, r]): Result[Int32, Int32] \ r with Order[k] =
        if (start > end)
            bug!("`searchLeaf` should always find a key")
        else
            let mid = (start + end) / 2;
            let midKey = Array.get(mid, keys);
            match k <=> midKey {
                case Comparison.LessThan => if (start == mid) Result.Err(mid) else searchLeaf(k, start, mid - 1, keys)
                case Comparison.EqualTo  => Result.Ok(mid)
                case _                   => if (end == mid) Result.Err(mid + 1) else searchLeaf(k, mid + 1, end, keys)
            }

    ///
    /// Insert `v` at index `i` in an array `arr` with `c` elements, shifting the elements to the right,
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
    /// Shift the elements of an array `arr` with `c` elements to the right, starting at index `i` + 1.
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

    def _isOverflow(node: Node[k, v, r]): Bool \ r =
        match node {
            case Node.Inner(c, keys, _, _, _) => deref c == Array.length(keys)
            case Node.Leaf(c, keys, _, _, _)  => deref c == Array.length(keys)
        }

    /// Split a node into two nodes, returning the new node and the key to be inserted in the parent.
    /// The splitted node and the new node will have the same parent, hence a new root will be created if necessary.
    /// TODO not so elegant to have the two optional parameters
    def splitNode!(rc: Region[r], node: Node[k, v, r], overflowedKey: k, overflowedVal: Option[v], overflowedChild: Option[Node[k, v, r]]): (Node[k, v, r], k) \ r with ToString[k], ToString[v] = match node {
        case Node.Leaf(c, keys, vals, right, parent) =>
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
                case _ => ()
            };
            (newNode, Array.get(0, newKeys))
        case Node.Inner(c, keys, children, right, parent) =>
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
            c := mid;
            right := Some(newNode);
            match deref parent {
                case None =>
                    let newRootChildren = Array.empty(rc, cc + 1);
                    Array.put(node, 0, newRootChildren);
                    let newRoot = Node.Inner(Ref.fresh(rc, 0), Array.empty(rc, cc), newRootChildren, Ref.fresh(rc, None), Ref.fresh(rc, None));
                    parent := Some(newRoot);
                    newParent := Some(newRoot)
                case _ => ()
            };
            (newNode, Array.get(mid, keys))
    }

    def copyOfRange!(src: Array[v, r], start: Int32, end: Int32, dest: Array[v, r], destStart: Int32): Unit \ r =
        if (start >= end) ()
        else
            let v = Array.get(start, src);
            Array.put(v, destStart, dest);
            copyOfRange!(src, start + 1, end, dest, destStart + 1)

    def insertInner!(rc: Region[r], k: k, n: Node[k, v, r], node: Node[k, v, r]): Unit \ r with Order[k], ToString[k], ToString[v] = match node {
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

    @Test
    pub def testSplit01(): Bool \ IO = region rc {
        let t = BPlusTree.empty(rc, 5);
        BPlusTree.insert(20, 20, t);
        BPlusTree.insert(10, 10, t);
        BPlusTree.insert(30, 30, t);
        BPlusTree.insert(25, 25, t);
        let BPlusTree.BPlusTree(_, root) = t;
        let (newNode, newKey) = BPlusTree.splitNode!(rc, deref root, 35, Some(35), None);
        println(nodeToString(deref root));
        println(nodeToString(newNode));
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

    @Test
    pub def testSplit02(): Bool \ IO = region rc {
        let t = empty(rc, 5);
        insert(20, 20, t);
        insert(10, 10, t);
        insert(30, 30, t);
        insert(25, 25, t);
        insert(15, 15, t);
        insert(5, 5, t);
        insert(18, 18, t);
        insert(3, 3, t);
        let BPlusTree.BPlusTree(_, root) = t;
        println(nodeToString(deref root));
        match deref root {
            case Node.Inner(c, _, children, _, _) =>
                let validChildren = Array.toVector(children) |> Vector.slice(start = 0, end = deref c);
                foreach (child <- validChildren) {
                    println(nodeToString(child))
                };
                true
            case _ => false
        }
    }

}
