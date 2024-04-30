mod Concurrent2 {
    use Concurrent2.ReentrantReadWriteLock

    pub enum BLinkTree[k: Type, v: Type, r: Region](Region[r], Ref[Node[k, v, r], r])

    /// level | key_count | keys | children/values | right_sibling | lock
    /// The last element of `keys` is the high key.
    enum Node[k: Type, v: Type, r: Region] {
        case Inner(Int32, Ref[Int32, r], Array[k, r], Array[Node[k, v, r], r], Ref[Node[k, v, r], r], ReentrantReadWriteLock[r])
        case  Leaf(Int32, Ref[Int32, r], Array[k, r], Array[v, r],             Ref[Node[k, v, r], r], ReentrantReadWriteLock[r])
        case Empty
    }

    // TODO do we need atomicity for deref?
    /// we do need that change of the root ref be seen by all threads immediately

    mod BLinkTree {

        use Concurrent2.Node
        use Concurrent2.ReentrantReadWriteLock

        pub def empty(rc: Region[r], m: Int32): BLinkTree[k, v, r] \ r =
            BLinkTree(rc, Ref.fresh(rc, Node.Leaf(
                0,
                Ref.fresh(rc, 0),
                Array.empty(rc, m),
                Array.empty(rc, m - 1),
                Ref.fresh(rc, Node.Empty),
                ReentrantReadWriteLock.newLock(rc, false)
            )))

        pub def isEmpty(t: BLinkTree[k, v, r]): Bool \ r =
            let BLinkTree(_, root) = t;
            match deref root {
                case Node.Leaf(_, c, _, _, _, _) =>
                    Ref.get(c) == 0
                case _ => false
            }

        ///
        /// Returns a string representation of the B+Tree.
        ///
        /// Each line represents a level of the tree, with the root at the top.
        ///
        pub def toString(t: BLinkTree[k, v, r]): String \ r with ToString[k], ToString[v] =
            if (isEmpty(t))
                "[]"
            else
                let BLinkTree(rc, root) = t;
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
            case Node.Inner(_, _, keys, _, right, _) =>
                "[" +
                Array.join(" ", validKeys(rc, node)) + " " +
                (if (isNull(deref right)) "Inf" else ToString.toString(Array.get(Array.length(keys) - 1, keys))) +
                "]"
            case Node.Leaf(_, _, keys, _, right, _) =>
                "[" +
                Array.join(" ", Array.zip(rc, validKeys(rc, node), validVals(rc, node))) + " " +
                (if (isNull(deref right)) "Inf" else ToString.toString(Array.get(Array.length(keys) - 1, keys))) +
                "]"
            case Node.Empty => ""
        }

        def validKeys(rc: Region[r], node: Node[k, v, r]): Array[k, r] \ r = match node {
            case Node.Inner(_, c, keys, _, _, _) => Array.slice(rc, start = 0, end = deref c, keys)
            case Node.Leaf(_, c, keys, _, _, _)  => Array.slice(rc, start = 0, end = deref c, keys)
            case Node.Empty => Array.empty(rc, 0)
        }

        def validVals(rc: Region[r], node: Node[k, v, r]): Array[v, r] \ r = match node {
            case Node.Inner(_, _, _, _, _, _)   => Array.empty(rc, 0)
            case Node.Leaf(_, c, _, vals, _, _) => Array.slice(rc, start = 0, end = deref c, vals)
            case Node.Empty => Array.empty(rc, 0)
        }

        def validChildren(rc: Region[r], node: Node[k, v, r]): Array[Node[k, v, r], r] \ r = match node {
            case Node.Inner(_, c, _, children, _, _) => Array.slice(rc, start = 0, end = deref c + 1, children)
            case Node.Leaf(_, _, _, _, _, _)         => Array.empty(rc, 0)
            case Node.Empty => Array.empty(rc, 0)
        }

        pub def get(k: k, t: BLinkTree[k, v, r]): Option[v] \ r with Order[k] =
            let BLinkTree(_, root) = t;
            let (leaf, _) = descendToLeaf(k, deref root, Nil);
            let (node, i) = searchLeaf(k, leaf, false);
            match node {
                case Node.Leaf(_, c, keys, values, _, lock) =>
                    if (i < deref c and k == Array.get(i, keys)) {
                        let ret = Some(Array.get(i, values));
                        ReentrantReadWriteLock.unlockRead(lock);
                        ret
                    } else {
                        ReentrantReadWriteLock.unlockRead(lock);
                        None
                    }
                case _ => bug!("searchLeaf returned a non-leaf node")
            }

        pub def put!(k: k, v: v, t: BLinkTree[k, v, r]): Unit \ r with Order[k], ToString[k], ToString[v] =
            let BLinkTree(rc, root) = t;
            let (leaf, stack) = descendToLeaf(k, deref root, Nil);
            insertLeaf!(rc, k, v, leaf, stack, root)

        pub def putAll!(kvs: m[(k, v)], t: BLinkTree[k, v, r]): Unit \ r with Order[k], Foldable[m], ToString[k], ToString[v] =
            Foldable.forEach(kv -> put!(fst(kv), snd(kv), t), kvs)

        def insertLeaf!(rc: Region[r], k: k, v: v, node: Node[k, v, r], stack: List[Node[k, v, r]], root: Ref[Node[k, v, r], r]): Unit \ r with Order[k], ToString[k], ToString[v] =
            let (dstNode, i) = searchLeaf(k, node, true);
            // unchecked_cast(println("dstNode: ${nodeToString(rc, dstNode)}, ${i}") as Unit \ r);
            match dstNode {
                case Node.Leaf(level, c, keys, vals, right, lock) =>
                    let cc = deref c;
                    if (i < cc and k == Array.get(i, keys)) {
                        Array.put(v, i, vals);
                        ReentrantReadWriteLock.unlockWrite(lock)
                    } else {
                        match (arrayInsert!(k, i, cc, Array.length(keys) - 1, keys), arrayInsert!(v, i, cc, Array.length(vals), vals)) {
                            case (Some(overflowedKey), Some(overflowedVal)) =>
                                let (upKey, upChild) = splitLeaf(rc, keys, vals, c, right, level, overflowedKey, overflowedVal);
                                ReentrantReadWriteLock.unlockWrite(lock);
                                match stack {
                                    case Nil =>
                                        let rootLevel = getRootLevel(root);
                                        if (rootLevel == level) {
                                            let newKeys = Array.empty(rc, cc + 1);
                                            let newChildren = Array.empty(rc, cc + 1);
                                            Array.put(upKey, 0, newKeys);
                                            Array.put(node, 0, newChildren);
                                            Array.put(upChild, 1, newChildren);
                                            setHighKey!(newKeys, overflowedKey);
                                            let newRoot = Node.Inner(level + 1, Ref.fresh(rc, 1), newKeys, newChildren, Ref.fresh(rc, Node.Empty), ReentrantReadWriteLock.newLock(rc, false));
                                            root := newRoot
                                        } else {
                                            redescend!(rc, upKey, upChild, level, root)
                                        }
                                    case parent :: restStack =>
                                        insertInner!(rc, upKey, upChild, parent, restStack, root)
                                }
                            case (None, None) =>
                                c := cc + 1;
                                if (i == cc and not isNull(deref right))
                                    setHighKey!(keys, Array.get(cc, keys))
                                else
                                    ();
                                ReentrantReadWriteLock.unlockWrite(lock)
                            case _ => bug!("`arrayInsert` should always return either both or none")
                        }
                    }
                case _ => bug!("searchLeaf returned a non-leaf node")
            }

        def insertInner!(rc: Region[r], k: k, child: Node[k, v, r], node: Node[k, v, r], stack: List[Node[k, v, r]], root: Ref[Node[k, v, r], r]): Unit \ r with Order[k] =
            let (dstNode, i) = searchInnerForInsert(k, node);
            match dstNode {
                case Node.Inner(level, c, keys, children, right, lock) =>
                    let cc = deref c;
                    match (arrayInsert!(k, i, cc, Array.length(keys) - 1, keys), arrayInsert!(child, i + 1, cc + 1, Array.length(children), children)) {
                        case (Some(overflowedKey), Some(overflowedChild)) =>
                            let (upKey, upChild) = splitInner(rc, keys, children, c, right, level, overflowedKey, overflowedChild);
                            ReentrantReadWriteLock.unlockWrite(lock);
                            match stack {
                                case Nil =>
                                    let rootLevel = getRootLevel(root);
                                    if (rootLevel == level) {
                                        let newKeys = Array.empty(rc, cc + 1);
                                        let newChildren = Array.empty(rc, cc + 1);
                                        Array.put(upKey, 0, newKeys);
                                        Array.put(node, 0, newChildren);
                                        Array.put(upChild, 1, newChildren);
                                        setHighKey!(newKeys, overflowedKey);
                                        let newRoot = Node.Inner(level + 1, Ref.fresh(rc, 1), newKeys, newChildren, Ref.fresh(rc, Node.Empty), ReentrantReadWriteLock.newLock(rc, false));
                                        root := newRoot
                                    } else {
                                        redescend!(rc, upKey, upChild, level, root)
                                    }
                                case parent :: restStack =>
                                    insertInner!(rc, upKey, upChild, parent, restStack, root)
                            }
                        case (None, None) =>
                            c := cc + 1;
                            ReentrantReadWriteLock.unlockWrite(lock)
                        case _ => bug!("`arrayInsert` should always return either both or none")
                    }
                case _ => bug!("searchInnerForInsert returned a non-inner node")
            }

        def redescend!(rc: Region[r], k: k, child: Node[k, v, r], toLevel: Int32, root: Ref[Node[k, v, r], r]): Unit \ r with Order[k] =
            let (node, stack) = descendToLevel(k, toLevel, deref root, Nil);
            insertInner!(rc, k, child, node, stack, root)

        def getRootLevel(root: Ref[Node[k, v, r], r]): Int32 \ r =
            match deref root {
                case Node.Leaf(level, _, _, _, _, _) => level
                case Node.Inner(level, _, _, _, _, _) => level
                case Node.Empty => bug!("root should never be empty")
            }

        def getHighKey(keys: Array[k, r]): k \ r =
            Array.get(Array.length(keys) - 1, keys)

        def setHighKey!(keys: Array[k, r], k: k): Unit \ r =
            Array.put(k, Array.length(keys) - 1, keys)
        
        def descendToLeaf(k: k, node: Node[k, v, r], stack: List[Node[k, v, r]]): (Node[k, v, r], List[Node[k, v, r]]) \ r with Order[k] =
            match node {
                case Node.Leaf(_, _, _, _, _, _) =>
                    (node, stack)
                case Node.Inner(_, _, _, _, _, _) =>
                    let (child, new_stack) = searchInner(k, node, stack);
                    descendToLeaf(k, child, new_stack)
                case _ => bug!("should never receive Node.Empty")
            }

        def descendToLevel(k: k, toLevel: Int32, node: Node[k, v, r], stack: List[Node[k, v, r]]): (Node[k, v, r], List[Node[k, v, r]]) \ r with Order[k] =
            match node {
                case Node.Inner(level, _, _, _, _, _) =>
                    if (level == toLevel + 1) {
                        (node, stack)
                    } else {
                        let (child, new_stack) = searchInner(k, node, stack);
                        descendToLevel(k, level, child, new_stack)
                    }
                case _ => bug!("should never receive a leaf node")
            }

        def isNull(node: Node[k, v, r]): Bool = match node {
            case Node.Empty => true
            case _ => false
        }

        def needToGoRight(k: k, keys: Array[k, r], right: Node[k, v, r]): Bool \ r with Order[k] =
            not isNull(right) and k > getHighKey(keys)

        ///
        /// Returns the child node to follow and the stack with the node from the current level pushed.
        ///
        def searchInner(k: k, node: Node[k, v, r], stack: List[Node[k, v, r]]): (Node[k, v, r], List[Node[k, v, r]]) \ r with Order[k] =
            match node {
                case Node.Inner(_, c, keys, children, right, lock) =>
                    ReentrantReadWriteLock.lockRead(lock);
                    if (needToGoRight(k, keys, deref right)) {
                        ReentrantReadWriteLock.unlockRead(lock);
                        searchInner(k, deref right, stack)
                    } else {
                        let cc = deref c - 1;
                        let i = findFirstGreaterOrEqual(k, 0, cc, keys);
                        let child = Array.get(i, children);
                        ReentrantReadWriteLock.unlockRead(lock);
                        (child, node :: stack)
                    }
                case _ => bug!("searchInner called with a non-inner node")
            }

        ///
        /// Returns a locked node and the index of `k` or where `k` should be inserted.
        ///
        def searchLeaf(k: k, node: Node[k, v, r], isWriting: Bool): (Node[k, v, r], Int32) \ r with Order[k] =
            match node {
                case Node.Leaf(_, c, keys, _, right, lock) =>
                    if (isWriting) ReentrantReadWriteLock.lockWrite(lock)
                    else ReentrantReadWriteLock.lockRead(lock);
                    if (deref c == 0) {
                        (node, 0)
                    } else {
                        if (needToGoRight(k, keys, deref right)) {
                            if (isWriting) ReentrantReadWriteLock.unlockWrite(lock)
                            else ReentrantReadWriteLock.unlockRead(lock);
                            searchLeaf(k, deref right, isWriting)
                        } else {
                            let i = findFirstGreaterOrEqual(k, 0, deref c - 1, keys);
                            (node, i)
                        }
                    }
                case _ => bug!("searchLeaf called with a non-leaf node")
            }

        ///
        /// Returns a locked node and the index of where `k` should be inserted.
        ///
        def searchInnerForInsert(k: k, node: Node[k, v, r]): (Node[k, v, r], Int32) \ r with Order[k] =
            match node {
                case Node.Inner(_, c, keys, _, right, lock) =>
                    ReentrantReadWriteLock.lockWrite(lock);
                    if (needToGoRight(k, keys, deref right)) {
                        ReentrantReadWriteLock.unlockWrite(lock);
                        searchInnerForInsert(k, deref right)
                    } else {
                        let i = findFirstGreaterOrEqual(k, 0, deref c - 1, keys);
                        (node, i)
                    }
                case _ => bug!("searchInnerForInsert called with a non-inner node")
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

        ///
        /// Insert `v` at index `i` in an array `arr` with capacity `cap`
        /// where `[0, c)` are valid elements, shifting the elements to the right,
        /// possibly returning the overflowed element.
        ///
        def arrayInsert!(v: v, i: Int32, c: Int32, cap: Int32, arr: Array[v, r]): Option[v] \ r =
            if (c == cap and i == c)
                Some(v)
            else
                let ret = arrayShiftRight!(i, c, cap, arr);
                Array.put(v, i, arr);
                ret

        ///
        /// Shift the elements of an array `arr` with capacity `cap`
        /// where `[0, c)` are valid elements to the right, starting at index `i` + 1.
        /// If an element is shifted out of the array, return it.
        ///
        def arrayShiftRight!(i: Int32, c: Int32, cap: Int32, arr: Array[v, r]): Option[v] \ r =
            def loop(ii, cc) = {
                if (ii >= cc) ()
                else
                    let last = Array.get(cc - 1, arr);
                    Array.put(last, cc, arr);
                    loop(ii, cc - 1)
            };
            if (c == cap) {
                let ret = Array.nth(c - 1, arr);
                loop(i, c - 1);
                ret
            } else {
                loop(i, c);
                None
            }

        ///
        /// Splits a full leaf node, where `k` is the overflowed key and `v` is the overflowed value.
        /// Returns the new key and node to be inserted in the parent.
        ///
        def splitLeaf(rc: Region[r], keys: Array[k, r], vals: Array[v, r], c: Ref[Int32, r], right: Ref[Node[k, v, r], r], level: Int32, k: k, v: v): (k, Node[k, v, r]) \ r =
            let cc = deref c;
            let mid = (cc + 1) / 2;
            let newKeys = Array.empty(rc, cc + 1);
            let newVals = Array.empty(rc, cc);
            let newRight = Ref.fresh(rc, deref right);
            copyOfRange!(keys, mid, cc, newKeys, 0);
            copyOfRange!(vals, mid, cc, newVals, 0);
            Array.put(k, cc - mid, newKeys);
            Array.put(v, cc - mid, newVals);
            setHighKey!(newKeys, k);
            setHighKey!(keys, Array.get(mid - 1, keys));
            let newNode = Node.Leaf(level, Ref.fresh(rc, cc - mid + 1), newKeys, newVals, newRight, ReentrantReadWriteLock.newLock(rc, false));
            c := mid;
            right := newNode;
            (Array.get(mid - 1, keys), newNode)

        ///
        /// Splits a full inner node, where `k` is the overflowed key and `v` is the overflowed child.
        /// Returns the new key and node to be inserted in the parent.
        ///
        def splitInner(rc: Region[r], keys: Array[k, r], children: Array[Node[k, v, r], r], c: Ref[Int32, r], right: Ref[Node[k, v, r], r], level: Int32, k: k, child: Node[k, v, r]): (k, Node[k, v, r]) \ r =
            let cc = deref c;
            let mid = (cc + 1) / 2;
            let newKeys = Array.empty(rc, cc + 1);
            let newChildren = Array.empty(rc, cc + 1);
            let newRight = Ref.fresh(rc, deref right);
            copyOfRange!(keys, mid + 1, cc, newKeys, 0);
            copyOfRange!(children, mid + 1, cc + 1, newChildren, 0);
            Array.put(k, cc - mid - 1, newKeys);
            Array.put(child, cc - mid, newChildren);
            setHighKey!(newKeys, getHighKey(keys));
            setHighKey!(keys, Array.get(mid, keys));
            let newNode = Node.Inner(level, Ref.fresh(rc, cc - mid), newKeys, newChildren, newRight, ReentrantReadWriteLock.newLock(rc, false));
            c := mid;
            right := newNode;
            (Array.get(mid, keys), newNode)

        ///
        /// Copy the elements of array `src` from index `start`(inclusive) to `end`(exclusive) to an array `dest` starting at index `destStart`.
        ///
        def copyOfRange!(src: Array[v, r], start: Int32, end: Int32, dest: Array[v, r], destStart: Int32): Unit \ r =
            if (start >= end) ()
            else
                let v = Array.get(start, src);
                Array.put(v, destStart, dest);
                copyOfRange!(src, start + 1, end, dest, destStart + 1)
        }
}
