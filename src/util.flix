pub def genFacts[v: Type](n: Int32, g: Random -> v \ IO, seed: Option[Int64]): List[v] \ IO =
    let r = Random.newWithSeed(Option.getWithDefault(0i64, seed));
    List.repeat(n, 0) |>
    List.map(_ -> g(r))

pub def genFact1(max: Int32, r: Random): Int32 \ IO =
    Random.nextNatWithMax(r, max)

pub def genFact2(max: Int32, r: Random): (Int32, Int32) \ IO =
    (Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max))

pub def genFact3(max: Int32, r: Random): (Int32, Int32, Int32) \ IO =
    (Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max))

pub def genFact4(max: Int32, r: Random): (Int32, Int32, Int32, Int32) \ IO =
    (Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max))

pub def genFact5(max: Int32, r: Random): (Int32, Int32, Int32, Int32, Int32) \ IO =
    (Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max))

pub def genFact6(max: Int32, r: Random): (Int32, Int32, Int32, Int32, Int32, Int32) \ IO =
    (Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max), Random.nextNatWithMax(r, max))