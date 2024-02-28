mod Concurrent2 {
    use Concurrent2.ReentrantReadWriteLock

    pub enum BPlusTree[k: Type, v: Type, r: Region](Region[r], Ref[Node[k, v, r], r])

    /// key_count | keys | children/values | high_key | right_sibling | lock
    enum Node[k: Type, v: Type, r: Region] {
        case Inner(Ref[Int32, r], Array[k, r], Array[Node[k, v, r], r], Ref[Option[k], r], Ref[Option[Node[k, v, r]], r], ReentrantReadWriteLock[r])
        case  Leaf(Ref[Int32, r], Array[k, r], Array[v, r],             Ref[Option[k], r], Ref[Option[Node[k, v, r]], r], ReentrantReadWriteLock[r])
    }

    // TODO do we need atomicity for deref

    mod BPlusTree {

        use Concurrent2.Node
        use Concurrent2.ReentrantReadWriteLock

        pub def empty(rc: Region[r], m: Int32): BPlusTree[k, v, r] \ r =
            BPlusTree(rc, Ref.fresh(rc, Node.Leaf(
                Ref.fresh(rc, 0),
                Array.empty(rc, m - 1),
                Array.empty(rc, m - 1),
                Ref.fresh(rc, None),
                Ref.fresh(rc, None),
                ReentrantReadWriteLock.newLock(rc, false)
            )))

        pub def isEmpty(t: BPlusTree[k, v, r]): Bool \ r =
            let BPlusTree(_, root) = t;
            match deref root {
                case Node.Leaf(c, _, _, _, _, _) =>
                    Ref.get(c) == 0
                case _ => false
            }

        pub def get(k: k, t: BPlusTree[k, v, r]): Option[v] \ r with Order[k] =
            let BPlusTree(_, root) = t;
            let (leaf, _) = descendToLeaf(k, deref root, Nil);
            let (node, i) = searchLeaf(k, leaf, false);
            match node {
                case Node.Leaf(c, keys, values, _, _, lock) =>
                    if (i < deref c and k == Array.get(i, keys)) {
                        let ret = Some(Array.get(i, values));
                        ReentrantReadWriteLock.unlockRead(lock);
                        ret
                    } else {
                        ReentrantReadWriteLock.unlockRead(lock);
                        None
                    }
                case _ => bug!("searchLeaf returned an inner node")
            }

        def descendToLeaf(k: k, node: Node[k, v, r], stack: List[Node[k, v, r]]): (Node[k, v, r], List[Node[k, v, r]]) \ r with Order[k] =
            match node {
                case Node.Leaf(_, _, _, _, _, _) =>
                    (node, stack)
                case Node.Inner(_, _, _, _, _, _) =>
                    let (child, new_stack) = searchInner(k, node, stack);
                    descendToLeaf(k, child, new_stack)
            }

        def searchInner(k: k, node: Node[k, v, r], stack: List[Node[k, v, r]]): (Node[k, v, r], List[Node[k, v, r]]) \ r with Order[k] =
            match node {
                case Node.Leaf(_, _, _, _, _, _) => bug!("searchInner called with a leaf node")
                case Node.Inner(c, keys, children, highKey, right, lock) =>
                    ReentrantReadWriteLock.lockRead(lock);
                    match deref highKey {
                        case None => bug!("highKey is None")
                        case Some(hk) =>
                            if (k > hk) {
                                match deref right {
                                    case None => bug!("right sibling is None")
                                    case Some(r) =>
                                        ReentrantReadWriteLock.unlockRead(lock);
                                        searchInner(k, r, stack)
                                }
                            } else {
                                let i = findFirstGreaterOrEqual(k, 0, deref c - 1, keys);
                                let child = Array.get(i, children);
                                ReentrantReadWriteLock.unlockRead(lock);
                                (child, node :: stack)
                            }
                    }
            }
        
        ///
        /// Returns a locked node and the index of `k` or where `k` should be inserted.
        ///
        def searchLeaf(k: k, node: Node[k, v, r], isWriting: Bool): (Node[k, v, r], Int32) \ r with Order[k] =
            match node {
                case Node.Inner(_, _, _, _, _, _) => bug!("searchLeaf called with an inner node")
                case Node.Leaf(c, keys, _, highKey, right, lock) =>
                    if (isWriting) ReentrantReadWriteLock.lockWrite(lock)
                    else ReentrantReadWriteLock.lockRead(lock);
                    match deref highKey {
                        case None => bug!("highKey is None")
                        case Some(hk) =>
                            if (k > hk) {
                                match deref right {
                                    case None => bug!("right sibling is None")
                                    case Some(r) =>
                                        if (isWriting) ReentrantReadWriteLock.unlockWrite(lock)
                                        else ReentrantReadWriteLock.unlockRead(lock);
                                        searchLeaf(k, r, isWriting)
                                }
                            } else {
                                let i = findFirstGreaterOrEqual(k, 0, deref c - 1, keys);
                                (node, i)
                            }
                    }
            }

        def findFirstGreaterOrEqual(k: k, i: Int32, j: Int32, keys: Array[k, r]): Int32 \ r with Order[k] =
            if (i > j) {
                i
            } else {
                let m = (i + j) / 2;
                if (k <= Array.get(m, keys))
                    findFirstGreaterOrEqual(k, i, m - 1, keys)
                else
                    findFirstGreaterOrEqual(k, m + 1, j, keys)
            }

        pub def put!(k: k, v: v, t: BPlusTree[k, v, r]): Unit \ r with Order[k] =
            let BPlusTree(rc, root) = t;
            let (leaf, stack) = descendToLeaf(k, deref root, Nil);
            let (node, i) = searchLeaf(k, leaf, true);
            match node {
                case Node.Leaf(c, keys, vals, highKey, right, lock) =>
                    let cc = deref c;
                    if (i < cc and k == Array.get(i, keys)) {
                        Array.put(v, i, vals);
                        ReentrantReadWriteLock.unlockWrite(lock)
                    } else {
                        match (arrayInsert!(k, i, cc, keys), arrayInsert!(v, i, cc, vals)) {
                            case (Some(overflowedKey), Some(overflowedVal)) =>
                                ???
                            case (None, None) =>
                                c := cc + 1;
                                highKey := Some(Array.get(cc, keys));
                                ReentrantReadWriteLock.unlockWrite(lock)
                            case _ => bug!("`arrayInsert` should always return either both or none")
                        }
                    }
                case _ => bug!("searchLeaf returned an inner node")
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
    }
}