mod MyBench.Trees {
    def genFacts1(size: Int32): Vector[Int32] \ IO =
        genFacts(size, genFact1(size * 10), None)

    def inputSizes(): Vector[Int32] =
        Vector.range(2, 21) |> Vector.map(Int32.pow(2))

    def bTreeNodeSizes(): Vector[Int32] =
        Vector.range(4, 8) |> Vector.map(Int32.pow(2))

    pub def testRBTInsert(): Unit \ IO = region rc {
        println("RedBlackTree insertion");
        let inputSizes = Vector.range(15, 22) |> Vector.map(Int32.pow(2));
        let testSize = 100;
        foreach (size <- inputSizes) {
            let r = Random.fresh();
            let input = Vector.repeat(size, 0) |> Vector.map(_ -> Random.nextInt32(r));
            let m = MutMap.empty(rc);
            foreach (x <- input) {
                MutMap.put!(x, x, m)
            };
            let t = Time.Epoch.nanoseconds();
            Vector.repeat(testSize, 0) |> Vector.forEach(_ -> {
                let k = Random.nextInt32(r);
                MutMap.put!(k, k, m)
            });
            println("${Time.Epoch.nanoseconds() - t}")
        }
    }

    pub def testBTreeInsert(): Unit \ IO = region rc {
        foreach(nodeSize <- bTreeNodeSizes()) {
            println("${nodeSize}-way BPlusTree insertion");
            let inputSizes = Vector.range(15, 22) |> Vector.map(Int32.pow(2));
            let testSize = 100;
            foreach (size <- inputSizes) {
                let r = Random.fresh();
                let input = Vector.repeat(size, 0) |> Vector.map(_ -> Random.nextInt32(r));
                let m = BPlusTree.empty(rc, nodeSize);
                foreach (x <- input) {
                    BPlusTree.put!(x, x, m)
                };
                let t = Time.Epoch.nanoseconds();
                Vector.repeat(testSize, 0) |> Vector.forEach(_ -> {
                    let k = Random.nextInt32(r);
                    BPlusTree.put!(k, k, m)
                });
                println("${Time.Epoch.nanoseconds() - t}")
            }
        }
    }

    pub def testRBTRangeQuery(): Unit \ IO = region rc {
        println("RedBlackTree range query");
        let input = Vector.range(1, Int32.pow(2, 20)) |> Vector.shuffle(Random.fresh());
        let m = MutMap.empty(rc);
        foreach(x <- input) {
            MutMap.put!(x, x, m)
        };

        let rangeSize = Vector#{Int32.pow(2, 6), Int32.pow(2, 10), Int32.pow(2, 14), Int32.pow(2, 18)};
        foreach(size <- rangeSize) {
            let left = Int32.pow(2, 19);
            let right = left + size;
            let p = k ->
                if (k < left) Comparison.LessThan
                else if (k > right) Comparison.GreaterThan
                else Comparison.EqualTo;
            let result = Ref.fresh(rc, 0);
            let t = Time.Epoch.nanoseconds();
            MutMap.rangeQueryWith(p, _ -> _ -> result := deref result + 1, m);
            println("${Time.Epoch.nanoseconds() - t}")
        }
    }

    pub def testBTreeRangeQuery(): Unit \ IO = region rc {
        foreach(nodeSize <- bTreeNodeSizes()) {
            println("${nodeSize}-way BPlusTree range query");
            let input = Vector.range(1, Int32.pow(2, 20)) |> Vector.shuffle(Random.fresh());
            let m = BPlusTree.empty(rc, nodeSize);
            foreach(x <- input) {
                BPlusTree.put!(x, x, m)
            };

            let rangeSize = Vector#{Int32.pow(2, 6), Int32.pow(2, 10), Int32.pow(2, 14), Int32.pow(2, 18)};
            foreach(size <- rangeSize) {
                let left = Int32.pow(2, 19);
                let right = left + size;
                let p = k ->
                    if (k < left) Comparison.LessThan
                    else if (k > right) Comparison.GreaterThan
                    else Comparison.EqualTo;
                let result = Ref.fresh(rc, 0);
                let t = Time.Epoch.nanoseconds();
                BPlusTree.rangeQueryWith(p, _ -> _ -> result := deref result + 1, m);
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
