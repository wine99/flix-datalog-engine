use BPlusTree.empty
use BPlusTree.put!
use BPlusTree.get
use BPlusTree.toString


@Test
def testBPTree01(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    // println(toString(t));
    toString(t) == "[(20, 20)] "
    and get(20, t) == Some(20)
}

@Test
def testBPTree02(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    // println(toString(t));
    toString(t) == "[(10, 10) (20, 20)] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
}

@Test
def testBPTree03(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    put!(30, 30, t);
    // println(toString(t));
    toString(t) == "[(10, 10) (20, 20) (30, 30)] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
}

@Test
def testBPTree04(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    put!(30, 30, t);
    put!(25, 25, t);
    toString(t) == "[(10, 10) (20, 20) (25, 25) (30, 30)] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
    and get(25, t) == Some(25)
}

@Test
def testBPTree05(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    put!(30, 30, t);
    put!(25, 25, t);
    put!(15, 15, t);
    toString(t) == "[15] \n[(10, 10) (15, 15)] [(20, 20) (25, 25) (30, 30)] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
    and get(25, t) == Some(25)
    and get(15, t) == Some(15)
}


@Test
def testBPTree06(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    put!(30, 30, t);
    put!(25, 25, t);
    put!(15, 15, t);
    put!(5, 5, t);
    // println(toString(t));
    toString(t) == "[15] \n[(5, 5) (10, 10) (15, 15)] [(20, 20) (25, 25) (30, 30)] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
    and get(25, t) == Some(25)
    and get(15, t) == Some(15)
    and get(5, t) == Some(5)
}

@Test
def testBPTree07(): Bool = region rc {
    let t = empty(rc, 5);
    put!(20, 20, t);
    put!(10, 10, t);
    put!(30, 30, t);
    put!(25, 25, t);
    put!(15, 15, t);
    put!(5, 5, t);
    put!(18, 18, t);
    put!(3, 3, t);
    toString(t) == "[15] \n[(3, 3) (5, 5) (10, 10) (15, 15)] [(18, 18) (20, 20) (25, 25) (30, 30)] "
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
def testBPTree08(): Bool = region rc {
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
    toString(t) == "[5 15 19] \n[(3, 3) (5, 5)] [(10, 10) (13, 13) (15, 15)] [(18, 18) (19, 19)] [(20, 20) (25, 25) (30, 30)] "
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
def testBPTree09(): Bool = region rc {
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
    toString(t) == "[5 15 19 25] \n[(3, 3) (5, 5)] [(10, 10) (13, 13) (15, 15)] [(18, 18) (19, 19)] [(20, 20) (25, 25)] [(28, 28) (30, 30) (40, 40)] "
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
def testBPTree10(): Bool = region rc {
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
    put!(34, 34, t);
    put!(36, 36, t);
    toString(t) == "[15] \n[5 11] [19 25 30] \n[(3, 3) (5, 5)] [(10, 10) (11, 11)] [(12, 12) (13, 13) (15, 15)] [(16, 16) (17, 17) (18, 18) (19, 19)] [(20, 20) (25, 25)] [(28, 28) (30, 30)] [(34, 34) (36, 36) (40, 40)] "
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
def testBPTree11(): Bool = region rc {
    let t = empty(rc, 5);
    let keys = Vector#{95, 12, 491, 924, 950, 229, 814, 577, 710, 584, 197, 614, 103, 800, 915, 741, 399, 830, 395, 276, 292, 979, 910, 654, 471, 35, 562, 992, 758, 307, 707, 932, 207, 727, 357, 616, 458, 303, 576, 92, 680, 299, 579, 171, 199, 183, 519, 166, 777, 585};
    let kvs = Vector.map(k -> (k, k), keys);
    BPlusTree.putAll!(kvs, t);
    // println(toString(t));
    Vector.map(k -> get(k, t) == Some(k), keys)
    |> Vector.foldLeft(b -> a -> b and a, true)
}

@Test
def testBPTree12(): Bool = region rc {
    let t = empty(rc, 4);
    let keys = Vector#{95, 12, 491, 924, 950, 229, 814, 577, 710, 584, 197, 614, 103, 800, 915, 741, 399, 830, 395, 276, 292, 979, 910, 654, 471, 35, 562, 992, 758, 307, 707, 932, 207, 727, 357, 616, 458, 303, 576, 92, 680, 299, 579, 171, 199, 183, 519, 166, 777, 585};
    let kvs = Vector.map(k -> (k, k), keys);
    BPlusTree.putAll!(kvs, t);
    // println(toString(t));
    Vector.map(k -> get(k, t) == Some(k), keys)
    |> Vector.foldLeft(b -> a -> b and a, true)
}

@Test
def testBPTree13(): Bool = region rc {
    let t = empty(rc, 3);
    let keys = Vector#{95, 12, 491, 924, 950, 229, 814, 577, 710, 584, 197, 614, 103, 800, 915, 741, 399, 830, 395, 276, 292, 979, 910, 654, 471, 35, 562, 992, 758, 307, 707, 932, 207, 727, 357, 616, 458, 303, 576, 92, 680, 299, 579, 171, 199, 183, 519, 166, 777, 585};
    let kvs = Vector.map(k -> (k, k), keys);
    BPlusTree.putAll!(kvs, t);
    // println(toString(t));
    Vector.map(k -> get(k, t) == Some(k), keys)
    |> Vector.foldLeft(b -> a -> b and a, true)
}

@Test
def testBPTreeRangeQuery01(): Bool \ IO = region rc {
    let t = empty(rc, 20);
    let kvs = Vector.range(1, 5000) |> Vector.map(k -> (k, k)) |> Vector.shuffle(Random.fresh()) |> Vector.take(1000);
    BPlusTree.putAll!(kvs, t);
    let p = k ->
        if (k > 3000) Comparison.GreaterThan
        else if (k < 2000) Comparison.LessThan
        else Comparison.EqualTo;
    let r = BPlusTree.rangeQuery(p, (_, v) -> v, t);
    List.length(r) == Vector.count(kv -> p(fst(kv)) == Comparison.EqualTo, kvs)
}

@Test
def testBPTreeRangeQuery02(): Bool = region rc {
    let t = empty(rc, 4);
    let keys = Vector#{95, 12, 491, 924, 950, 229, 814, 577, 710, 584, 197, 614, 103, 800, 915, 741, 399, 830, 395, 276, 292, 979, 910, 654, 471, 35, 562, 992, 758, 307, 707, 932, 207, 727, 357, 616, 458, 303, 576, 92, 680, 299, 579, 171, 199, 183, 519, 166, 777, 585};
    let kvs = Vector.map(k -> (k, k), keys);
    BPlusTree.putAll!(kvs, t);
    let p = k ->
        if (k > 491) Comparison.GreaterThan
        else if (k < 276) Comparison.LessThan
        else Comparison.EqualTo;
    let r = BPlusTree.rangeQuery(p, (_, v) -> v, t) |> List.toVector;
    r == Vector.filter(k -> 276 <= k and k <= 491, keys) |> Vector.sort
}

@Test
def testBPTreeGetOrElsePut(): Bool = region rc {
    let t = empty(rc, 10);
    let kvs = Vector.range(1, 1000) |> Vector.map(k -> (k, k));
    foreach ((k, v) <- kvs) {
        BPlusTree.getOrElsePut!(k, v, t);
        ()
    };
    let vs: Vector[Int32] =
        foreach ((k, v) <- kvs)
            yield BPlusTree.getOrElsePut!(k, v, t);
    Vector.length(vs) == Vector.length(kvs)
}
