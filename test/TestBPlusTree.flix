use BPlusTree.empty
use BPlusTree.insert
use BPlusTree.get
use BPlusTree.toString


@Test
def testBPTree01(): Bool \ IO = region rc {
    let t = empty(rc, 5);
    insert(20, 20, t);
    println(toString(t));
    toString(t) == "[(20, 20)] "
    and get(20, t) == Some(20)
}

@Test
def testBPTree02(): Bool \ IO = region rc {
    let t = empty(rc, 5);
    insert(20, 20, t);
    insert(10, 10, t);
    println(toString(t));
    toString(t) == "[(10, 10) (20, 20)] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
}

@Test
def testBPTree03(): Bool \ IO = region rc {
    let t = empty(rc, 5);
    insert(20, 20, t);
    insert(10, 10, t);
    insert(30, 30, t);
    println(toString(t));
    toString(t) == "[(10, 10) (20, 20) (30, 30)] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
}

@Test
def testBPTree04(): Bool \ IO = region rc {
    let t = empty(rc, 5);
    insert(20, 20, t);
    insert(10, 10, t);
    insert(30, 30, t);
    insert(25, 25, t);
    println(toString(t));
    toString(t) == "[(10, 10) (20, 20) (25, 25) (30, 30)] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
    and get(25, t) == Some(25)
}

@Test
def testBPTree05(): Bool \ IO = region rc {
    let t = empty(rc, 5);
    insert(20, 20, t);
    insert(10, 10, t);
    insert(30, 30, t);
    insert(25, 25, t);
    insert(15, 15, t);
    println(toString(t));
    toString(t) == "[20] \n[(10, 10) (15, 15)] [(20, 20) (25, 25) (30, 30)] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
    and get(25, t) == Some(25)
    and get(15, t) == Some(15)
}


@Test
def testBPTree06(): Bool \ IO = region rc {
    let t = empty(rc, 5);
    insert(20, 20, t);
    insert(10, 10, t);
    insert(30, 30, t);
    insert(25, 25, t);
    insert(15, 15, t);
    insert(5, 5, t);
    println(toString(t));
    toString(t) == "[20] \n[(5, 5) (10, 10) (15, 15)] [(20, 20) (25, 25) (30, 30)] "
    and get(10, t) == Some(10)
    and get(20, t) == Some(20)
    and get(30, t) == Some(30)
    and get(25, t) == Some(25)
    and get(15, t) == Some(15)
    and get(5, t) == Some(5)
}

@Test
def testBPTree07(): Bool \ IO = region rc {
    let t = empty(rc, 5);
    insert(20, 20, t);
    insert(10, 10, t);
    insert(30, 30, t);
    insert(25, 25, t);
    insert(15, 15, t);
    insert(5, 5, t);
    insert(18, 18, t);
    insert(3, 3, t);
    println(toString(t));
    toString(t) == "[10 20] \n[(3, 3) (5, 5)] [(10, 10) (15, 15) (18, 18)] [(20, 20) (25, 25) (30, 30)] "
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
def testBPTree08(): Bool \ IO = region rc {
    let t = empty(rc, 5);
    insert(20, 20, t);
    insert(10, 10, t);
    insert(30, 30, t);
    insert(25, 25, t);
    insert(15, 15, t);
    insert(5, 5, t);
    insert(18, 18, t);
    insert(3, 3, t);
    insert(13, 13, t);
    insert(19, 19, t);
    println(toString(t));
    toString(t) == "[10 15 20] \n[(3, 3) (5, 5)] [(10, 10) (13, 13)] [(15, 15) (18, 18) (19, 19)] [(20, 20) (25, 25) (30, 30)] "
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
def testBPTree09(): Bool \ IO = region rc {
    let t = empty(rc, 5);
    insert(20, 20, t);
    insert(10, 10, t);
    insert(30, 30, t);
    insert(25, 25, t);
    insert(15, 15, t);
    insert(5, 5, t);
    insert(18, 18, t);
    insert(3, 3, t);
    insert(13, 13, t);
    insert(19, 19, t);
    insert(28, 28, t);
    insert(40, 40, t);
    println(toString(t));
    toString(t) == "[10 15 20 28] \n[(3, 3) (5, 5)] [(10, 10) (13, 13)] [(15, 15) (18, 18) (19, 19)] [(20, 20) (25, 25)] [(28, 28) (30, 30) (40, 40)] "
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
def testBPTree10(): Bool \ IO = region rc {
    let t = empty(rc, 5);
    insert(20, 20, t);
    insert(10, 10, t);
    insert(30, 30, t);
    insert(25, 25, t);
    insert(15, 15, t);
    insert(5, 5, t);
    insert(18, 18, t);
    insert(3, 3, t);
    insert(13, 13, t);
    insert(19, 19, t);
    insert(28, 28, t);
    insert(40, 40, t);
    insert(17, 17, t);
    insert(16, 16, t);
    println(toString(t));
    toString(t) == "[17] \n[10 15] [20 28] \n[(3, 3) (5, 5)] [(10, 10) (13, 13)] [(15, 15) (16, 16)] [(17, 17) (18, 18) (19, 19)] [(20, 20) (25, 25)] [(28, 28) (30, 30) (40, 40)] "
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
def testBPTree11(): Bool \ IO = region rc {
    let t = empty(rc, 5);
    let keys = Vector#{95, 12, 491, 924, 950, 229, 814, 577, 710, 584, 197, 614, 103, 800, 915, 741, 399, 830, 395, 276, 292, 979, 910, 654, 471, 35, 562, 992, 758, 307, 707, 932, 207, 727, 357, 616, 458, 303, 576, 92, 680, 299, 579, 171, 199, 183, 519, 166, 777, 585};
    foreach (k <- keys) {
        insert(k, k, t)
    };
    println(toString(t));
    Vector.map(k -> get(k, t) == Some(k), keys)
    |> Vector.foldLeft(b -> a -> b and a, true)
}

def testBPTree12(): Bool \ IO = region rc {
    let t = empty(rc, 4);
    let keys = Vector#{95, 12, 491, 924, 950, 229, 814, 577, 710, 584, 197, 614, 103, 800, 915, 741, 399, 830, 395, 276, 292, 979, 910, 654, 471, 35, 562, 992, 758, 307, 707, 932, 207, 727, 357, 616, 458, 303, 576, 92, 680, 299, 579, 171, 199, 183, 519, 166, 777, 585};
    foreach (k <- keys) {
        insert(k, k, t);
        println(toString(t))
    };
    println(toString(t));
    Vector.map(k -> get(k, t) == Some(k), keys)
    |> Vector.foldLeft(b -> a -> b and a, true)
}
