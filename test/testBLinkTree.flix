use Concurrent2.BLinkTree.empty
use Concurrent2.BLinkTree.put!
use Concurrent2.BLinkTree.putAll!
use Concurrent2.BLinkTree.get
use Concurrent2.BLinkTree.size
use Concurrent2.BLinkTree.toString
use Concurrent2.BLinkTree.rangeQuery
use Concurrent2.BLinkTree.getOrElsePut!


pub def putAllConcurrent(rc: Region[r1], kvs: m[(k, v)], t: Concurrent2.BLinkTree[k, v, r], threads: Int32): Unit \ { r1, IO } with Order[k], Foldable[m], ToString[k], ToString[v] =
    foreach (_ <- Vector.range(0, threads))
        spawn putAll!(kvs, t) @ rc

pub def putAllPartition(rc: Region[r1], kvs: Vector[(k, v)], t: Concurrent2.BLinkTree[k, v, r], threads: Int32): Unit \ { r1, IO } with Order[k], ToString[k], ToString[v] =
    let n = Vector.length(kvs);
    let m = n / threads;
    def loop(i, end) = {
        if (i < end)
            let (k, v) = Vector.get(i, kvs);
            put!(k, v, t);
            loop(i + 1, end)
        else ()
    };
    foreach (i <- Vector.range(0, threads)) {
        let start = i * m;
        let end = if (i == threads - 1) n else (i + 1) * m;
        spawn loop(start, end) @ rc
    }

pub def randomRead(t: Concurrent2.BLinkTree[Int32, Int32, r], max: Int32, iteration: Int32): Unit \ { IO, r } =
    if (iteration == 0)
        ()
    else
        let k = Random.nextNatWithMax(Random.fresh(), max);
        let _ = get(k, t);
        randomRead(t, max, iteration - 1)

pub def repeatTest(fn: Unit -> Bool \ r, n: Int32): Bool \ r =
    Vector.range(0, n)
    |> Vector.map(_ -> fn())
    |> Vector.foldLeft(b -> a -> b and a, true)

@Test
def testBLTree00(): Bool = region rc {
    let t: Concurrent2.BLinkTree[Int32, Int32, rc] = empty(rc, 5);
    toString(t) == "[]" and Concurrent2.BLinkTree.isEmpty(t)
    and get(20, t) == None
}

@Test
def testBLTree01(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    // println(toString(t));
    toString(t) == "[(20, 20) Inf] "
    and get(20, t) == Some(20)
}

@Test
def testBLTree02(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    // println(toString(t));
    toString(t) == "[(10, 10) (20, 20) Inf] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
}

@Test
def testBLTree03(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    put!(30, 30, t);
    // println(toString(t));
    toString(t) == "[(10, 10) (20, 20) (30, 30) Inf] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
}

@Test
def testBLTree04(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    put!(30, 30, t);
    put!(25, 25, t);
    toString(t) == "[(10, 10) (20, 20) (25, 25) (30, 30) Inf] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
    and get(25, t) == Some(25)
}

@Test
def testBLTree05(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    put!(30, 30, t);
    put!(25, 25, t);
    put!(15, 15, t);
    toString(t) == "[15 Inf] \n[(10, 10) (15, 15) 15] [(20, 20) (25, 25) (30, 30) Inf] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
    and get(25, t) == Some(25)
    and get(15, t) == Some(15)
}


@Test
def testBLTree06(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    put!(30, 30, t);
    put!(25, 25, t);
    put!(15, 15, t);
    put!(5, 5, t);
    // println(toString(t));
    toString(t) == "[15 Inf] \n[(5, 5) (10, 10) (15, 15) 15] [(20, 20) (25, 25) (30, 30) Inf] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
    and get(25, t) == Some(25)
    and get(15, t) == Some(15)
    and get(5, t) == Some(5)
}

@Test
def testBLTree07(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    put!(30, 30, t);
    put!(25, 25, t);
    put!(15, 15, t);
    put!(5, 5, t);
    put!(18, 18, t);
    put!(3, 3, t);
    toString(t) == "[15 Inf] \n[(3, 3) (5, 5) (10, 10) (15, 15) 15] [(18, 18) (20, 20) (25, 25) (30, 30) Inf] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
    and get(25, t) == Some(25)
    and get(15, t) == Some(15)
    and get(5, t) == Some(5)
    and get(18, t) == Some(18)
    and get(3, t) == Some(3)
}


@Test
def testBLTree08(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    put!(30, 30, t);
    put!(25, 25, t);
    put!(15, 15, t);
    put!(5, 5, t);
    put!(18, 18, t);
    put!(3, 3, t);
    // println(toString(t));
    put!(13, 13, t);
    // println(toString(t));
    put!(19, 19, t);
    // println(toString(t));
    toString(t) == "[5 15 19 Inf] \n[(3, 3) (5, 5) 5] [(10, 10) (13, 13) (15, 15) 15] [(18, 18) (19, 19) 19] [(20, 20) (25, 25) (30, 30) Inf] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
    and get(25, t) == Some(25)
    and get(15, t) == Some(15)
    and get(5, t) == Some(5)
    and get(18, t) == Some(18)
    and get(3, t) == Some(3)
    and get(13, t) == Some(13)
    and get(19, t) == Some(19)
}

@Test
def testBLTree09(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    put!(30, 30, t);
    put!(25, 25, t);
    put!(15, 15, t);
    put!(5, 5, t);
    put!(18, 18, t);
    put!(3, 3, t);
    put!(13, 13, t);
    put!(19, 19, t);
    put!(28, 28, t);
    put!(40, 40, t);
    // println(toString(t));
    toString(t) == "[5 15 19 25 Inf] \n[(3, 3) (5, 5) 5] [(10, 10) (13, 13) (15, 15) 15] [(18, 18) (19, 19) 19] [(20, 20) (25, 25) 25] [(28, 28) (30, 30) (40, 40) Inf] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
    and get(25, t) == Some(25)
    and get(15, t) == Some(15)
    and get(5, t) == Some(5)
    and get(18, t) == Some(18)
    and get(3, t) == Some(3)
    and get(13, t) == Some(13)
    and get(19, t) == Some(19)
    and get(28, t) == Some(28)
    and get(40, t) == Some(40)
}

@Test
def testBLTree10(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    put!(30, 30, t);
    put!(25, 25, t);
    put!(15, 15, t);
    put!(5, 5, t);
    put!(18, 18, t);
    put!(3, 3, t);
    put!(13, 13, t);
    put!(19, 19, t);
    put!(28, 28, t);
    put!(40, 40, t);
    put!(17, 17, t);
    put!(16, 16, t);
    put!(11, 11, t);
    put!(12, 12, t);
    // println(toString(t));
    put!(34, 34, t);
    put!(36, 36, t);
    // println(toString(t));
    toString(t) == "[15 Inf] \n[5 11 15] [19 25 30 Inf] \n[(3, 3) (5, 5) 5] [(10, 10) (11, 11) 11] [(12, 12) (13, 13) (15, 15) 15] [(16, 16) (17, 17) (18, 18) (19, 19) 19] [(20, 20) (25, 25) 25] [(28, 28) (30, 30) 30] [(34, 34) (36, 36) (40, 40) Inf] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
    and get(25, t) == Some(25)
    and get(15, t) == Some(15)
    and get(5, t) == Some(5)
    and get(18, t) == Some(18)
    and get(3, t) == Some(3)
    and get(13, t) == Some(13)
    and get(19, t) == Some(19)
    and get(28, t) == Some(28)
    and get(40, t) == Some(40)
    and get(17, t) == Some(17)
    and get(16, t) == Some(16)
}

@Test
def testBLTree11(): Bool = region rc {
    let t = empty(rc, 5);
    let keys = Vector#{95, 12, 491, 924, 950, 229, 814, 577, 710, 584, 197, 614, 103, 800, 915, 741, 399, 830, 395, 276, 292, 979, 910, 654, 471, 35, 562, 992, 758, 307, 707, 932, 207, 727, 357, 616, 458, 303, 576, 92, 680, 299, 579, 171, 199, 183, 519, 166, 777, 585};
    let kvs = Vector.map(k -> (k, k), keys);
    putAll!(kvs, t);
    // println(toString(t));
    Vector.map(k -> get(k, t) == Some(k), keys)
    |> Vector.foldLeft(b -> a -> b and a, true)
    and size(t) == Vector.length(keys)
}

@Test
def testBLTree12(): Bool = region rc {
    let t = empty(rc, 4);
    let keys = Vector#{95, 12, 491, 924, 950, 229, 814, 577, 710, 584, 197, 614, 103, 800, 915, 741, 399, 830, 395, 276, 292, 979, 910, 654, 471, 35, 562, 992, 758, 307, 707, 932, 207, 727, 357, 616, 458, 303, 576, 92, 680, 299, 579, 171, 199, 183, 519, 166, 777, 585};
    let kvs = Vector.map(k -> (k, k), keys);
    putAll!(kvs, t);
    // println(toString(t));
    Vector.map(k -> get(k, t) == Some(k), keys)
    |> Vector.foldLeft(b -> a -> b and a, true)
}

@Test
def testBLTree13(): Bool = region rc {
    let t = empty(rc, 3);
    let keys = Vector#{95, 12, 491, 924, 950, 229, 814, 577, 710, 584, 197, 614, 103, 800, 915, 741, 399, 830, 395, 276, 292, 979, 910, 654, 471, 35, 562, 992, 758, 307, 707, 932, 207, 727, 357, 616, 458, 303, 576, 92, 680, 299, 579, 171, 199, 183, 519, 166, 777, 585};
    let kvs = Vector.map(k -> (k, k), keys);
    putAll!(kvs, t);
    // println(toString(t));
    Vector.map(k -> get(k, t) == Some(k), keys)
    |> Vector.foldLeft(b -> a -> b and a, true)
}

@Test
def testBLTree14(): Bool \ IO = region rc {
    let keys = Vector.range(1, Int32.pow(2, 14));
    def test(): Bool = {
        let kvs = Vector.shuffle(Random.fresh(), keys) |> Vector.map(k -> (k, k));
        let t = empty(rc, 20);
        putAll!(kvs, t);
        Vector.map(k -> get(k, t) == Some(k), keys)
        |> Vector.foldLeft(b -> a -> b and a, true)
        and size(t) == Vector.length(keys)
    };
    repeatTest(test, 20)
}

@Test
def testBLTreeConcurrent01(): Bool \ IO = region rc {
    let keys = Vector#{95, 12, 491, 924, 950, 229, 814, 577, 710, 584, 197, 614, 103, 800, 915, 741, 399, 830, 395, 276, 292, 979, 910, 654, 471, 35, 562, 992, 758, 307, 707, 932, 207, 727, 357, 616, 458, 303, 576, 92, 680, 299, 579, 171, 199, 183, 519, 166, 777, 585};
    let kvs = Vector.map(k -> (k, k), keys);
    def test(): Bool = {
        let t = empty(rc, 5);
        region r1 {
            putAllConcurrent(r1, kvs, t, 6)
        };
        // println(toString(t));
        Vector.map(k -> get(k, t) == Some(k), keys)
        |> Vector.foldLeft(b -> a -> b and a, true)
    };
    repeatTest(test, 20)
}

@Test
def testBLTreeConcurrent02(): Bool \ IO = region rc {
    let keys = Vector.range(1, Int32.pow(2, 16));
    def test(): Bool = {
        let kvs = Vector.shuffle(Random.fresh(), keys) |> Vector.map(k -> (k, k));
        let t = empty(rc, 20);
        region r1 {
            putAllConcurrent(r1, kvs, t, 6)
        };
        Vector.map(k -> get(k, t) == Some(k), keys)
        |> Vector.foldLeft(b -> a -> b and a, true)
        and size(t) == Vector.length(keys)
    };
    repeatTest(test, 20)
}

@Test
def testBLTreeConcurrent03(): Bool \ IO = region rc {
    let keys = Vector.range(1, Int32.pow(2, 20));
    def test(): Bool = {
        let kvs = Vector.shuffle(Random.fresh(), keys) |> Vector.map(k -> (k, k));
        let t = empty(rc, 20);
        region r1 {
            putAllPartition(r1, kvs, t, 6)
        };
        Vector.map(k -> get(k, t) == Some(k), keys)
        |> Vector.foldLeft(b -> a -> b and a, true)
        and size(t) == Vector.length(keys)
    };
    repeatTest(test, 20)
}

@Test
def testBLTreeConcurrent04(): Bool \ IO = region rc {
    def test(): Bool = {
        let keys = Vector.range(1, Int32.pow(2, Random.nextNatWithMax(Random.fresh(), 20)));
        let len = Vector.length(keys);
        let kvs = Vector.shuffle(Random.fresh(), keys) |> Vector.map(k -> (k, k));
        let t = empty(rc, 32);
        region r1 {
            putAllPartition(r1, kvs, t, 6);
            spawn randomRead(t, len * 2, len * 4) @ r1;
            spawn randomRead(t, len * 2, len * 4) @ r1;
            spawn randomRead(t, len * 2, len * 4) @ r1;
            spawn randomRead(t, len * 2, len * 4) @ r1;
            spawn randomRead(t, len * 2, len * 4) @ r1;
            spawn randomRead(t, len * 2, len * 4) @ r1
        };
        true
    };
    repeatTest(test, 50)
}

def testBLTreeForEach01(): Bool = region rc {
    let t = empty(rc, 5);
    let keys = Vector.range(1, 100);
    let kvs = keys |> Vector.map(k -> (k, k));
    putAll!(kvs, t);
    let sum = Ref.fresh(rc, 0);
    Concurrent2.BLinkTree.forEach((_, v) -> sum := deref sum + v, t);
    deref sum == Vector.foldLeft((a, b) -> a + b, 0, keys)
}

@Test
def testBLTreeConcurrent05(): Bool \ IO = region rc {
    def test(): Bool = {
        let keys = Vector.range(1, Int32.pow(2, Random.nextNatWithMax(Random.fresh(), 20)));
        let len = Vector.length(keys);
        let kvs = Vector.shuffle(Random.fresh(), keys) |> Vector.map(k -> (k, k));
        let t = empty(rc, 32);
        region r1 {
            putAllPartition(r1, kvs, t, 6);
            spawn randomRead(t, len * 2, len * 4) @ r1;
            spawn randomRead(t, len * 2, len * 4) @ r1;
            spawn randomRead(t, len * 2, len * 4) @ r1;
            spawn randomRead(t, len * 2, len * 4) @ r1;
            spawn randomRead(t, len * 2, len * 4) @ r1;
            spawn randomRead(t, len * 2, len * 4) @ r1
        };
        true
    };
    repeatTest(test, 50)
}

@Test
def testRangeQuery01(): Bool \ IO = region rc {
    let t = empty(rc, 20);
    let kvs = Vector.range(1, 5000) |> Vector.map(k -> (k, k)) |> Vector.shuffle(Random.fresh()) |> Vector.take(1000);
    putAll!(kvs, t);
    let p = k ->
        if (k > 3000) Comparison.GreaterThan
        else if (k < 2000) Comparison.LessThan
        else Comparison.EqualTo;
    let r = rangeQuery(p, (_, v) -> v, t);
    List.length(r) == Vector.count(kv -> p(fst(kv)) == Comparison.EqualTo, kvs)
}

@Test
def testRangeQuery02(): Bool = region rc {
    let t = empty(rc, 20);
    let p = k ->
        if (k > 3000) Comparison.GreaterThan
        else if (k < 2000) Comparison.LessThan
        else Comparison.EqualTo;
    let r = rangeQuery(p, (_, v) -> v, t);
    List.length(r) == 0
}

@Test
def testRangeQuery03(): Bool = region rc {
    let t = empty(rc, 4);
    let keys = Vector#{95, 12, 491, 924, 950, 229, 814, 577, 710, 584, 197, 614, 103, 800, 915, 741, 399, 830, 395, 276, 292, 979, 910, 654, 471, 35, 562, 992, 758, 307, 707, 932, 207, 727, 357, 616, 458, 303, 576, 92, 680, 299, 579, 171, 199, 183, 519, 166, 777, 585};
    let kvs = Vector.map(k -> (k, k), keys);
    putAll!(kvs, t);
    let p = k ->
        if (k > 491) Comparison.GreaterThan
        else if (k < 276) Comparison.LessThan
        else Comparison.EqualTo;
    let r = rangeQuery(p, (_, v) -> v, t) |> List.toVector;
    r == Vector.filter(k -> 276 <= k and k <= 491, keys) |> Vector.sort
}

@Test
def testGetOrElsePut(): Bool = region rc {
    let t = empty(rc, 10);
    let kvs = Vector.range(1, 10000) |> Vector.map(k -> (k, k));
    foreach ((k, v) <- kvs) {
        getOrElsePut!(k, v, t);
        ()
    };
    let vs: Vector[Int32] =
        foreach ((k, v) <- kvs)
            yield getOrElsePut!(k, v, t);
    Vector.length(vs) == Vector.length(kvs)
}

@Test
def testToMap(): Bool = region rc {
    let kvs = Vector.range(1, 10000) |> Vector.map(k -> (k, k));
    let m = Vector.toMap(kvs);
    let m2 = Concurrent2.BLinkTree.fromMap(rc, 32, m) |> Concurrent2.BLinkTree.toMap;
    m == m2
}