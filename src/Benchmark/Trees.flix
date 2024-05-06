mod MyBench.Trees {
    def genFacts1(size: Int32): Vector[Int32] \ IO =
        genFacts(size, genFact1(size), None)
    
    def inputSizes(): Vector[Int32] =
        Vector.range(2, 23) |> Vector.map(Int32.pow(2))

    def bTreeNodeSizes(): Vector[Int32] =
        Vector.range(5, 7) |> Vector.map(Int32.pow(2))

    pub def testRBTInsert(): Unit \ IO = region rc {
        println("RedBlackTree insertion");
        foreach(size <- inputSizes()) {
            let input = genFacts1(size);
            let t = Time.Epoch.nanoseconds();
            let m = MutMap.empty(rc);
            foreach(x <- input) {
                MutMap.put!(x, x, m)
            };
            println("${Time.Epoch.nanoseconds() - t}")
        }
    }

    pub def testBTreeInsert(): Unit \ IO = region rc {
        foreach(nodeSize <- bTreeNodeSizes()) {
            println("${nodeSize}-way BPlusTree insertion");
            foreach(size <- inputSizes()) {
                let input = genFacts1(size);
                let t = Time.Epoch.nanoseconds();
                let m = BPlusTree.empty(rc, nodeSize);
                foreach(x <- input) {
                    BPlusTree.put!(x, x, m)
                };
                println("${Time.Epoch.nanoseconds() - t}")
            }
        }
    }

    pub def testRBTRangeQuery(): Unit \ IO = region rc {
        foreach(size <- inputSizes()) {
            let input = genFacts1(size);
            let m = MutMap.empty(rc);
            foreach(x <- input) {
                MutMap.put!(x, x, m)
            };
            let p = k ->
                if (k > size / 4) Comparison.GreaterThan
                else if (k < size / 4 * 3) Comparison.LessThan
                else Comparison.EqualTo;
            let t = Time.Epoch.nanoseconds();
            MutMap.rangeQueryWith(p, _ -> _ -> (), m);
            println("RedBlackTree range query, ${size}, ${Time.Epoch.nanoseconds() - t}")
        }
    }

    pub def testBTreeRangeQuery(): Unit \ IO = region rc {
        foreach(nodeSize <- bTreeNodeSizes()) {
            println("${nodeSize}-way BPlusTree range query}");
            foreach(size <- inputSizes()) {
                let input = genFacts1(size);
                let m = BPlusTree.empty(rc, nodeSize);
                foreach(x <- input) {
                    BPlusTree.put!(x, x, m)
                };
                let p = k ->
                    if (k > size / 4) Comparison.GreaterThan
                    else if (k < size / 4 * 3) Comparison.LessThan
                    else Comparison.EqualTo;
                let t = Time.Epoch.nanoseconds();
                BPlusTree.rangeQueryWith(p, _ -> _ -> (), m);
                println("${Time.Epoch.nanoseconds() - t}")
            }
        }
    }

    pub def testRBTRangeQuery2(): Unit \ IO = region rc {
        foreach(size <- inputSizes()) {
            let input = genFacts1(size);
            let m = MutMap.empty(rc);
            foreach(x <- input) {
                MutMap.put!(x, x, m)
            };
            let p = k ->
                if (k > size / 4) Comparison.GreaterThan
                else if (k < size / 4 * 3) Comparison.LessThan
                else Comparison.EqualTo;
            let result = MutList.empty(rc);
            let t = Time.Epoch.nanoseconds();
            MutMap.rangeQueryWith(p, _ -> v -> MutList.push!(v, result), m);
            println("RedBlackTree range query 2, ${size}, ${Time.Epoch.nanoseconds() - t}")
        }
    }

    pub def testBTreeRangeQuery2(): Unit \ IO = region rc {
        foreach(nodeSize <- bTreeNodeSizes()) {
            println("${nodeSize}-way BPlusTree range query 2");
            foreach(size <- inputSizes()) {
                let input = genFacts1(size);
                let m = BPlusTree.empty(rc, nodeSize);
                foreach(x <- input) {
                    BPlusTree.put!(x, x, m)
                };
                let p = k ->
                    if (k > size / 4) Comparison.GreaterThan
                    else if (k < size / 4 * 3) Comparison.LessThan
                    else Comparison.EqualTo;
                let result = MutList.empty(rc);
                let t = Time.Epoch.nanoseconds();
                BPlusTree.rangeQueryWith(p, _ -> v -> MutList.push!(v, result), m);
                println("${Time.Epoch.nanoseconds() - t}")
            }
        }
    }

    pub def testBLTreeInsert(): Unit \ IO = region rc {
        let threads = Vector#{4, 6, Environment.getVirtualProcessors()};
        foreach(thread <- threads) {
            foreach(nodeSize <- bTreeNodeSizes()) {
                println("${nodeSize}-way BLinkTree insertion, ${thread} threads");
                foreach(size <- inputSizes()) {
                    let input = genFacts1(size);
                    let kvs = Vector.map(x -> (x, x), input);
                    let t = Time.Epoch.nanoseconds();
                    let m = Concurrent2.BLinkTree.empty(rc, nodeSize);
                    region r1 {
                        putAllPartition(r1, kvs, m, thread)
                    };
                    println("${Time.Epoch.nanoseconds() - t}")
                }
            }
        }
    }
}
