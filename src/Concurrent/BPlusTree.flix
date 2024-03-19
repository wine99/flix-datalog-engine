///
/// A `BPlusTree` contains: m (maxDegree) and a root `Node`
///
pub enum BPlusTree[k: Type, v: Type, r: Region](Ref[Node[k, v, r], r])

///
/// A `Node` contains: a count (of existing keys), keys, children or values (if inner or leaf)
/// a link to the right sibling, and a link to the parent
/// TODO implementation without the parent link? right-sibling to support deletion?
///
enum Node[k: Type, v: Type, r: Region] {
    case Inner(Int32, Array[k, r], Array[Node[k, v, r], r], Ref[Option[Node[k, v, r]], r], Ref[Option[Node[k, v, r]], r])
    case  Leaf(Int32, Array[k, r], Array[v, r],             Ref[Option[Node[k, v, r]], r], Ref[Option[Node[k, v, r]], r])
}

mod BPlusTree {
    pub def empty(rc: Region[r], m: Int32): BPlusTree[k, v, r] \ r =
        BPlusTree(ref Node.Leaf(0, Array.empty(rc, m - 1), Array.empty(rc, m), ref None @ rc, ref None @ rc) @ rc)

    pub def isEmpty(t: BPlusTree[k, v, r]): Bool \ r =
        let BPlusTree(root) = t;
        match deref root {
            case Node.Leaf(0, _, _, _, _) => true
            case _ => false
        }

    pub def get(k: k, t: BPlusTree[k, v, r]): Option[v] \ r with Order[k] =
        let BPlusTree(root) = t;
        let leaf = getLeaf(k, deref root);
        let (_, v) = searchNode(k, leaf);
        v

    pub def insert!(k: k, v: v, t: BPlusTree[k, v, r]): Unit \ r with Order[k] =
        let BPlusTree(root) = t;
        let leaf = getLeaf(k, deref root);
        let (i, vv) = searchNode(k, leaf);
        match leaf {
            case Node.Inner(_, _, _, _, _) => bug!("`getLeaf` should always return a leaf node")
            case Node.Leaf(c, keys, vals, _, _) => match vv {
                case Some(_) => Array.put(v, i, vals)
                case None(_) =>
                    if (not isOverflow(leaf)) {
                        arrayInsert!(k, i, c, keys);
                        arrayInsert!(v, i, c, vals)
                    } else {
                        // FIXME
                        // split the leaf
                        // call `insertInner` which might recurse and returns the node where the recursion stop
                        // if the root's parent is not none anymore, we let root point to the node returned by `insertInner`
                        ()
                    }
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
            match Array.nth(i, children) {
                case None => bug!("Array index out of bounds")
                case Some(c) => getLeaf(k, c)
            }
    }

    ///
    /// Search for a key in a node. For internal nodes, return the index
    /// of the child to follow and `None`. For leaf nodes, if the key exists,
    /// return the index and the value. If the key does not exist, return the index
    /// of where the key would be inserted and `None`.
    ///
    def searchNode(k: k, node: Node[k, v, r]): (Int32, Option[v]) \ r with Order[k] = match node {
        case Node.Inner(c, keys, _, _, _) => {
            (searchInner(k, 0, c - 1, keys), None)
        }
        case Node.Leaf(c, keys, vals, _, _) => {
            match searchLeaf(k, 0, c - 1, keys) {
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
            match Array.nth(mid, keys) {
                case None => bug!("Array index out of bounds")
                case Some(midKey) => match k <=> midKey {
                    case Comparison.LessThan => mid
                    case Comparison.EqualTo  => mid + 1
                    case _                   => searchInner(k, mid + 1, end, keys)
                }
            }

    def searchLeaf(k: k, start: Int32, end: Int32, keys: Array[k, r]): Result[Int32, Int32] \ r with Order[k] =
        let mid = (start + end) / 2;
        match Array.nth(mid, keys) {
            case None => bug!("Array index out of bounds")
            case Some(midKey) => match k <=> midKey {
                case Comparison.LessThan => if (start == end) Result.Err(mid) else searchLeaf(k, start, mid - 1, keys)
                case Comparison.EqualTo  => Result.Ok(mid)
                case _                   => if (start == end) Result.Err(mid + 1) else searchLeaf(k, mid + 1, end, keys)
            }
        }
    
    ///
    /// Insert `v` at index `i` in an array `arr` with `c` elements, shifting the elements to the right.
    ///
    def arrayInsert!(v: v, i: Int32, c: Int32, arr: Array[v, r]): Unit \ r =
        arrayShiftRight!(i, c, arr);
        Array.put(v, i, arr)

    ///
    /// Shift the elements of an array `arr` with `c` elements to the right, starting at index `i` + 1.
    ///
    def arrayShiftRight!(i: Int32, c: Int32, arr: Array[v, r]): Unit \ r =
        if (i >= c) ()
        else match Array.nth(c - 1, arr) {
            case None => bug!("Array index out of bounds")
            case Some(last) => 
                Array.put(last, c, arr);
                arrayShiftRight!(i, c - 1, arr)
        }

    def isOverflow(node: Node[k, v, r]): Bool =
        match node {
            case Node.Inner(c, keys, _, _, _) => c == Array.length(keys)
            case Node.Leaf(c, keys, _, _, _)  => c == Array.length(keys)
        }
    
}
