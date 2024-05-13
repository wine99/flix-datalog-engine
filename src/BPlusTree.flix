///
/// A `BPlusTree` contains: m (maxDegree) and a root `Node`
///
pub enum BPlusTree[k: Type, v: Type, r: Region](Region[r], Ref[Node[k, v, r], r])

///
/// A `Node` contains: a count (of existing keys), keys,
/// children or values (if inner or leaf), and
/// a link to the right sibling (if leaf).
///
/// TODO redistribution when inserting into a full node
///
enum Node[k: Type, v: Type, r: Region] {
    case Inner(Ref[Int32, r], Array[k, r], Array[Node[k, v, r], r])
    case  Leaf(Ref[Int32, r], Array[k, r], Array[v, r], Ref[Node[k, v, r], r])
    case Empty
}

mod BPlusTree {

    ///
    /// Returns a new `m`-way B+Tree, i.e. each node has capacity `m-1` and at most `m` children.
    ///
    pub def empty(rc: Region[r], m: Int32): BPlusTree[k, v, r] \ r =
        BPlusTree(rc, Ref.fresh(rc, Node.Leaf(Ref.fresh(rc, 0), Array.empty(rc, m - 1), Array.empty(rc, m - 1), Ref.fresh(rc, Node.Empty))))

    ///
    /// Returns `true` if and only if the tree is empty.
    ///
    pub def isEmpty(t: BPlusTree[k, v, r]): Bool \ r =
        let BPlusTree(_, root) = t;
        match deref root {
            case Node.Leaf(c, _, _, _) => Ref.get(c) == 0
            case _ => false
        }

    ///
    /// Returns `Some(v)` if `k -> v` is in the B+Tree `t`. Otherwise returns `None`.
    ///
    pub def get(k: k, t: BPlusTree[k, v, r]): Option[v] \ r with Order[k] =
        let BPlusTree(_, root) = t;
        let leaf = descendToLeaf(k, deref root);
        let i = searchNode(k, leaf);
        match leaf {
            case Node.Leaf(c, keys, vals, _) =>
                if (i == deref c or k != Array.get(i, keys)) None
                else Some(Array.get(i, vals))
            case _ => bug!("`descendToLeaf` should always return a leaf node")
        }

    ///
    /// Returns `v` if `k => v` is in `t`. Otherwise, returns `d`.
    ///
    pub def getWithDefault(k: k, d: v, t: BPlusTree[k, v, r]): v \ r with Order[k] =
        Option.getWithDefault(d, get(k, t))

    ///
    /// Returns `true` if and only if `t` contains the key `k`.
    ///
    pub def memberOf(k: k, t: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        not Option.isEmpty(get(k, t))

    ///
    /// Applies `f` to all key-value pairs from `t` where `p(k)` returns `Comparison.EqualTo`.
    ///
    /// The function `f` must be impure.
    ///
    pub def rangeQueryWith(p: k -> Comparison \ ef1, f: (k, v) -> Unit \ ef2, t: BPlusTree[k, v, r]): Unit \ { ef1, ef2, r } with Order[k] =
        if (isEmpty(t)) ()
        else
            let BPlusTree(_, root) = t;
            let leaf = descendToLeafWith(p, deref root);
            let index = searchNodeWith(p, leaf);
            walkLeafWith(p, leaf, index, f)

    ///
    /// Extracts a range of key-value pairs from `t`.
    ///
    /// That is, the result is a list of all pairs `(k, v)` where `p(k)` returns `Equal`.
    ///
    pub def rangeQuery(p: k -> Comparison \ ef1, f: (k, v) -> a \ ef2, t: BPlusTree[k, v, r]): List[a] \ { ef1, ef2, r } with Order[k] = region rc {
        let buffer = MutList.empty(rc);
        let g = k -> v -> MutList.push!(f(k, v), buffer);
        rangeQueryWith(p, g, t);
        MutList.toList(buffer)
    }

    ///
    /// Insert the binding `k -> v` into the B+Tree `t`, replacing the existing binding.
    ///
    pub def put!(k: k, v: v, t: BPlusTree[k, v, r]): Unit \ r with Order[k] =
        let BPlusTree(rc, root) = t;
        let (leaf, stack) = descendToLeafWithStack(k, deref root, Nil);
        let i = searchNode(k, leaf);
        match leaf {
            case Node.Leaf(c, keys, vals, _) =>
                if (i == deref c or k != Array.get(i, keys)) {
                    insertLeaf!(rc, k, v, i, leaf, stack, root)
                } else {
                    Array.put(v, i, vals)
                }
            case _ => bug!("`descendToLeaf` should always return a leaf node")
        }

    ///
    /// Inserts all key-value pairs in `kvs` into the B+Tree `t`, replacing existing bindings.
    ///
    pub def putAll!(kvs: m[(k, v)], t: BPlusTree[k, v, r]): Unit \ r with Order[k], Foldable[m] =
        Foldable.forEach(kv -> BPlusTree.put!(fst(kv), snd(kv), t), kvs)

    ///
    /// Returns `v'` if `k -> v'` is in the B+Tree `t`.
    ///
    /// Otherwise updates `t` with a new mapping `k -> v` and returns `v`.
    ///
    pub def getOrElsePut!(k: k, v: v, t: BPlusTree[k, v, r]): v \ r with Order[k] =
        let BPlusTree(rc, root) = t;
        let (leaf, stack) = descendToLeafWithStack(k, deref root, Nil);
        let i = searchNode(k, leaf);
        match leaf {
            case Node.Leaf(c, keys, vals, _) =>
                if (i == deref c or k != Array.get(i, keys)) {
                    insertLeaf!(rc, k, v, i, leaf, stack, root);
                    v
                } else {
                    Array.get(i, vals)
                }
            case _ => bug!("`descendToLeaf` should always return a leaf node")
        }

    ///
    /// Updates `t` with `k => f(k, v, v1)` if `k => v1` is in `t`.
    ///
    /// Otherwise, updates `t` with `k => v`.
    ///
    pub def putWith!(f: (v, v) -> v \ ef, k: k, v: v, t: BPlusTree[k, v, r]): Unit \ { ef, r } with Order[k] =
        let BPlusTree(rc, root) = t;
        let (leaf, stack) = descendToLeafWithStack(k, deref root, Nil);
        let i = searchNode(k, leaf);
        match leaf {
            case Node.Leaf(c, keys, vals, _) =>
                if (i == deref c or k != Array.get(i, keys)) {
                    insertLeaf!(rc, k, v, i, leaf, stack, root)
                } else {
                    let oldV = Array.get(i, vals);
                    Array.put(f(v, oldV), i, vals)
                }
            case _ => bug!("`descendToLeaf` should always return a leaf node")
        }

    ///
    /// Applies `f` to every key-value pair of `t`.
    ///
    pub def forEach(f: (k, v) -> Unit \ ef, t: BPlusTree[k, v, r]): Unit \ { ef, r } =
        let BPlusTree(_, root) = t;
        let leaf = leftmost(deref root);
        walkLeaf(leaf, 0, f)

    ///
    /// Merges the B+Tree `t1` into the B+Tree `t2` in a left-biased manner.
    ///
    /// That is, key collisions are resolved by taking the mapping from `t1`.
    ///
    pub def merge!(t1: BPlusTree[k, v, r1], t2: BPlusTree[k, v, r2]): Unit \ { r1, r2 } with Order[k] =
        forEach((k1, v1) -> put!(k1, v1, t2), t1)

    ///
    /// Merges the B+Tree `t1` into the B+Tree `t2` in a left-biased manner,
    /// solving collisions with the function `f` applied to `v1` and `v2`.
    ///
    pub def mergeWith!(f: (v, v) -> v \ ef, t1: BPlusTree[k, v, r1], t2: BPlusTree[k, v, r2]): Unit \ { ef, r1, r2 } with Order[k] =
        forEach((k1, v1) -> putWith!(f, k1, v1, t2), t1)

    ///
    /// Returns the B+Tree `t` as an immutable map.
    ///
    pub def toMap(t: BPlusTree[k, v, r]): Map[k, v] \ r with Order[k] = region rc {
        let m = MutMap.empty(rc);
        forEach((k, v) -> MutMap.put!(k, v, m), t);
        MutMap.toMap(m)
    }

    ///
    /// Returns a `m`-way B+Tree from a map.
    ///
    pub def fromMap(rc: Region[r], m: Int32, map: Map[k, v]): BPlusTree[k, v, r] \ r with Order[k] =
        let t = empty(rc, m);
        Map.forEach((k, v) -> put!(k, v, t), map);
        t

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
        case Node.Inner(_, _, _) =>
            "[" + Array.join(" ", validKeys(rc, node)) + "]"
        case Node.Leaf(_, _, _, _) =>
            "[" + Array.join(" ", Array.zip(rc, validKeys(rc, node), validVals(rc, node))) + "]"
        case Node.Empty => ""
    }

    ///
    /// Search down to a leaf node where the key would be found or inserted.
    /// Returns the leaf and a stack of (parent, the intermediate searcch result in the parent).
    ///
    def descendToLeafWithStack(k: k, node: Node[k, v, r], stack: List[(Node[k, v, r], Int32)]): (Node[k, v, r], List[(Node[k, v, r], Int32)]) \ r with Order[k] = match node {
        case Node.Leaf(_, _, _, _) =>
            (node, stack)
        case Node.Inner(_, _, children) =>
            let i = searchNode(k, node);
            descendToLeafWithStack(k, Array.get(i, children), (node, i) :: stack)
        case Node.Empty => bug!("Should not reach Node.Empty")
    }

    ///
    /// Search down to a leaf node where the key would be found or inserted.
    /// Returns the leaf.
    ///
    def descendToLeaf(k: k, node: Node[k, v, r]): Node[k, v, r] \ r with Order[k] = match node {
        case Node.Leaf(_, _, _, _) =>
            node
        case Node.Inner(_, _, children) =>
            let i = searchNode(k, node);
            descendToLeaf(k, Array.get(i, children))
        case Node.Empty => bug!("Should not reach Node.Empty")
    }

    ///
    /// Search down to a leaf node where the key would be found or inserted, using a predicate on keys.
    /// Returns the leaf.
    ///
    def descendToLeafWith(p: k -> Comparison \ ef, node: Node[k, v, r]): Node[k, v, r] \ { ef, r } with Order[k] = match node {
        case Node.Leaf(_, _, _, _) =>
            node
        case Node.Inner(_, _, children) =>
            let i = searchNodeWith(p, node);
            descendToLeafWith(p, Array.get(i, children))
        case Node.Empty => bug!("Should not reach Node.Empty")
    }

    ///
    /// Returns the leftmost leaf node of a subtree.
    ///
    def leftmost(node: Node[k, v, r]): Node[k, v, r] \ r = match node {
        case Node.Leaf(_, _, _, _) => node
        case Node.Inner(_, _, children) => leftmost(Array.get(0, children))
        case Node.Empty => bug!("Should not reach Node.Empty")
    }

    def searchNode(k: k, node: Node[k, v, r]): Int32 \ r with Order[k] =
        match node {
            case Node.Inner(c, keys, _) =>
                findFirstGreaterOrEqual(k, 0, deref c - 1, keys)
            case Node.Leaf(c, keys, _, _) =>
                findFirstGreaterOrEqual(k, 0, deref c - 1, keys)
            case Node.Empty => bug!("Should not reach Node.Empty")
        }

    def searchNodeWith(p: k -> Comparison \ ef, node: Node[k, v, r]): Int32 \ { ef, r } with Order[k] =
        match node {
            case Node.Inner(c, keys, _) =>
                findFirstGreaterOrEqualWith(p, 0, deref c - 1, keys)
            case Node.Leaf(c, keys, _, _) =>
                findFirstGreaterOrEqualWith(p, 0, deref c - 1, keys)
            case Node.Empty => bug!("Should not reach Node.Empty")
        }


    def findFirstGreaterOrEqual(k: k, i: Int32, j: Int32, keys: Array[k, r]): Int32 \ r with Order[k] =
        if (i > j)
            i
        else
            let mid = (i + j) / 2;
            let midKey = Array.get(mid, keys);
            if (midKey < k) findFirstGreaterOrEqual(k, mid + 1, j, keys)
            else findFirstGreaterOrEqual(k, i, mid - 1, keys)

    def findFirstGreaterOrEqualWith(p: k -> Comparison \ ef, i: Int32, j: Int32, keys: Array[k, r]): Int32 \ { ef, r } with Order[k] =
        if (i > j)
            i
        else
            let mid = (i + j) / 2;
            let midKey = Array.get(mid, keys);
            match p(midKey) {
                case Comparison.LessThan => findFirstGreaterOrEqualWith(p, mid + 1, j, keys)
                case _                   => findFirstGreaterOrEqualWith(p, i, mid - 1, keys)
            }

    ///
    /// Walks through the leaf level, starting from the index `start` of the leaf node `node`,
    /// applying `f` to each key-value pair, stops when `p(k)` does not return `Comparison.EqualTo`.
    ///
    def walkLeafWith(p: k -> Comparison \ ef1, node: Node[k, v, r], start: Int32, f: (k, v) -> Unit \ ef2): Unit \ { ef1, ef2, r } = match node {
        case Node.Leaf(c, keys, vals, right) =>
            walkLeafWithHelper(p, f, start, keys, vals, deref c, deref right)
        case Node.Inner(_, _, _) => bug!("`walkLeaf` should always receive a leaf node")
        case Node.Empty => bug!("Should not reach Node.Empty")
    }

    def walkLeafWithHelper(p: k -> Comparison \ ef1, f: (k, v) -> Unit \ ef2, start: Int32, keys: Array[k, r], vals: Array[v, r], c: Int32, right: Node[k, v, r]): Unit \ { ef1, ef2, r } =
        if (start < c) {
            let k = Array.get(start, keys);
            match p(k) {
                case Comparison.EqualTo =>
                    f(k, Array.get(start, vals));
                    walkLeafWithHelper(p, f, start + 1, keys, vals, c, right)
                case _ => ()
            }
        } else {
            match right {
                case Node.Empty => ()
                case Node.Leaf(c_, keys_, vals_, right_) => walkLeafWithHelper(p, f, 0, keys_, vals_, deref c_, deref right_)
                case Node.Inner(_, _, _) => bug!("A leaf node should always have a leaf node as its right sibling")
            }
        }

    ///
    /// Walks through the leaf level, starting from the index `start` of the leaf node `node`,
    /// applying `f` to each key-value pair.
    ///
    def walkLeaf(node: Node[k, v, r], start: Int32, f: (k, v) -> Unit \ ef2): Unit \ { ef1, ef2, r } = match node {
        case Node.Leaf(c, keys, vals, right) =>
            walkLeafHelper(f, start, keys, vals, deref c, deref right)
        case Node.Inner(_, _, _) => bug!("`walkLeaf` should always receive a leaf node")
        case Node.Empty => bug!("Should not reach Node.Empty")
    }

    def walkLeafHelper(f: (k, v) -> Unit \ ef2, start: Int32, keys: Array[k, r], vals: Array[v, r], c: Int32, right: Node[k, v, r]): Unit \ { ef1, ef2, r } =
        if (start < c) {
            f(Array.get(start, keys), Array.get(start, vals));
            walkLeafHelper(f, start + 1, keys, vals, c, right)
        } else {
            match right {
                case Node.Empty => ()
                case Node.Leaf(c_, keys_, vals_, right_) => walkLeafHelper(f, 0, keys_, vals_, deref c_, deref right_)
                case Node.Inner(_, _, _) => bug!("A leaf node should always have a leaf node as its right sibling")
            }
        }

    ///
    /// Insert `v` at index `i` in an array `arr` where `[0, c)` are valid elements, shifting the elements to the right,
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
    /// Shift the elements of an array `arr` where `[0, c)` are valid elements to the right, starting at index `i` + 1.
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

    ///
    /// Takes a full node and a pair of overflowed key-value (if the node is a leaf node)
    /// or a pair of overflowed key-child (if the node is an inner node), splits the node into two nodes,
    /// with the overflowed key-value or key-child placed in the new node, returns the new node and the key to be inserted in the parent.
    ///
    def splitNode!(rc: Region[r], node: Node[k, v, r], overflowedKey: k, overflowedVal: Option[v], overflowedChild: Option[Node[k, v, r]]): (Node[k, v, r], k) \ r = match node {
        case Node.Leaf(c, keys, vals, right) =>
            let cc = deref c;
            let mid = (cc + 1) / 2;
            let newKeys = Array.empty(rc, cc);
            let newVals = Array.empty(rc, cc);
            let newRight = Ref.fresh(rc, deref right);
            copyOfRange!(keys, mid, cc, newKeys, 0);
            copyOfRange!(vals, mid, cc, newVals, 0);
            Array.put(overflowedKey, cc - mid, newKeys);
            Array.put(match overflowedVal {
                case Some(v) => v
                case None    => bug!("`splitNode` should receive an overflowed value for a leaf node")
            }, cc - mid, newVals);
            let newNode = Node.Leaf(Ref.fresh(rc, cc - mid + 1), newKeys, newVals, newRight);
            c := mid;
            right := newNode;
            (newNode, Array.get(mid - 1, keys))
        case Node.Inner(c, keys, children) =>
            let cc = deref c;
            let mid = (cc + 1) / 2;
            let newKeys = Array.empty(rc, cc);
            let newChildren = Array.empty(rc, cc + 1);
            copyOfRange!(keys, mid + 1, cc, newKeys, 0);
            copyOfRange!(children, mid + 1, cc + 1, newChildren, 0);
            Array.put(overflowedKey, cc - mid - 1, newKeys);
            Array.put(match overflowedChild {
                case Some(child) => child
                case None    => bug!("`splitNode` should receive an overflowed child for an inner node")
            }, cc - mid, newChildren);
            let newNode = Node.Inner(Ref.fresh(rc, cc - mid), newKeys, newChildren);
            c := mid;
            (newNode, Array.get(mid, keys))
        case Node.Empty => bug!("Should not reach Node.Empty")
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
    /// Insert a key `k` and a val `v` into an leaf node `leaf`.
    ///
    /// When the `leaf` overflows, if the leaf has a parent, `insertInner` is called,
    /// otherwise a new root is created.
    ///
    def insertLeaf!(rc: Region[r], k: k, v: v, i: Int32, leaf: Node[k, v, r], stack: List[(Node[k, v, r], Int32)], root: Ref[Node[k, v, r], r]): Unit \ r with Order[k] =  match leaf {
        case Node.Inner(_, _, _) => bug!("`getLeaf` should always return a leaf node")
        case Node.Empty => bug!("Should not reach Node.Empty")
        case Node.Leaf(c, keys, vals, _) =>
            let cc = deref c;
            match (arrayInsert!(k, i, cc, keys), arrayInsert!(v, i, cc, vals)) {
                case (Some(overflowedKey), Some(overflowedVal)) =>
                    /// split the leaf
                    let (newLeaf, newKey) = splitNode!(rc, leaf, overflowedKey, Some(overflowedVal), None);
                    match stack {
                        case (parent, parent_i) :: rest =>
                            /// insert the new key into the parent
                            insertInner!(rc, newKey, newLeaf, parent_i, parent, rest, root)
                        case Nil =>
                            /// create a new root
                            let newRootKeys = Array.empty(rc, cc);
                            let newRootChildren = Array.empty(rc, cc + 1);
                            Array.put(leaf, 0, newRootChildren);
                            Array.put(newLeaf, 1, newRootChildren);
                            Array.put(newKey, 0, newRootKeys);
                            root := Node.Inner(Ref.fresh(rc, 1), newRootKeys, newRootChildren)
                    }
                case (None, None) =>
                    c := cc + 1
                case _ => bug!("`arrayInsert` should always return either both or none")
            }
    }

    ///
    /// Insert a key `k` and a node `n` into an inner node `node`.
    ///
    /// The operation might recurse if `node` overflows. If the root overflows, a new root is created.
    ///
    def insertInner!(rc: Region[r], k: k, n: Node[k, v, r], i: Int32, node: Node[k, v, r], stack: List[(Node[k, v, r], Int32)], root: Ref[Node[k, v, r], r]): Unit \ r with Order[k] = match node {
        case Node.Inner(c, keys, children) =>
            let cc = deref c;
            match (arrayInsert!(k, i, cc, keys), arrayInsert!(n, i + 1, cc + 1, children)) {
                case (Some(overflowedKey), Some(overflowedChild)) =>
                    let (newNode, newKey) = splitNode!(rc, node, overflowedKey, None, Some(overflowedChild));
                    match stack {
                        case (parent, parent_i) :: rest =>
                            /// insert the new key into the parent
                            insertInner!(rc, newKey, newNode, parent_i, parent, rest, root)
                        case Nil =>
                            /// create a new root
                            let newRootKeys = Array.empty(rc, cc);
                            let newRootChildren = Array.empty(rc, cc + 1);
                            Array.put(node, 0, newRootChildren);
                            Array.put(newNode, 1, newRootChildren);
                            Array.put(newKey, 0, newRootKeys);
                            root := Node.Inner(Ref.fresh(rc, 1), newRootKeys, newRootChildren)
                    }
                case (None, None) =>
                    c := deref c + 1
                case _ => bug!("`arrayInsert` should always return either both or none")
            }
        case _ => bug!("`insertInner!` should always receive an inner node")
    }

    def validKeys(rc: Region[r], node: Node[k, v, r]): Array[k, r] \ r = match node {
        case Node.Inner(c, keys, _) => Array.slice(rc, start = 0, end = deref c, keys)
        case Node.Leaf(c, keys, _, _)  => Array.slice(rc, start = 0, end = deref c, keys)
        case Node.Empty => Array.empty(rc, 0)
    }

    def validVals(rc: Region[r], node: Node[k, v, r]): Array[v, r] \ r = match node {
        case Node.Inner(_, _, _)   => Array.empty(rc, 0)
        case Node.Leaf(c, _, vals, _) => Array.slice(rc, start = 0, end = deref c, vals)
        case Node.Empty => Array.empty(rc, 0)
    }

    def validChildren(rc: Region[r], node: Node[k, v, r]): Array[Node[k, v, r], r] \ r = match node {
        case Node.Inner(c, _, children) => Array.slice(rc, start = 0, end = deref c + 1, children)
        case Node.Leaf(_, _, _, _)         => Array.empty(rc, 0)
        case Node.Empty => Array.empty(rc, 0)
    }

}
