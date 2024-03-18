///
/// A `BPlusTree` contains: m (maxDegree) and a root `Node`
///
pub enum BPlusTree[k: Type, v: Type, r: Region](Ref[Node[k, v, r], r])

///
/// A `Node` contains: a count (of existing keys), a link (to the right sibling), keys, and children or values (if inner or leaf)
///
enum Node[k: Type, v: Type, r: Region] {
    case Inner(Int32, Ref[Option[Node[k, v, r]], r], Array[k, r], Array[Node[k, v, r], r])
    case  Leaf(Int32, Ref[Option[Node[k, v, r]], r], Array[k, r], Array[v, r])
}

mod BPlusTree {
    pub def empty(rc: Region[r], m: Int32): BPlusTree[k, v, r] \ r =
        BPlusTree(ref Node.Leaf(0, ref None @ rc, Array.empty(rc, m - 1), Array.empty(rc, m)) @ rc)

    pub def isEmpty(t: BPlusTree[k, v, r]): Bool \ r =
        let BPlusTree(root) = t;
        match deref root {
            case Node.Leaf(0, _, _, _) => true
            case _ => false
        }

    pub def get(k: k, t: BPlusTree[k, v, r]): Option[v] \ r with Order[k] =
        let BPlusTree(root) = t;
        let leaf = getLeaf(k, deref root);
        let (_, v) = searchNode(k, leaf);
        v

    pub def insert!(k: k, v: v, t: BPlusTree[k, v, r]): Unit with Order[k] =
        ???

    ///
    /// Search down to a leaf node where the key would be found or inserted.
    ///
    def getLeaf(k: k, node: Node[k, v, r]): Node[k, v, r] \ r with Order[k] = match node {
        case Node.Leaf(_, _, _, _) => node
        case Node.Inner(_, _, _, children) =>
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
        case Node.Inner(c, _, keys, _) => {
            (searchInner(k, 0, c - 1, keys), None)
        }
        case Node.Leaf(c, _, keys, vals) => {
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
    
}
