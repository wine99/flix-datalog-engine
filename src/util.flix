pub def genFacts[v: Type](n: Int32, g: Random -> v \ IO, seed: Option[Int64]): Vector[v] \ IO =
    let r = Random.freshWithSeed(Option.getWithDefault(0i64, seed));
    Vector.repeat(n, 0) |>
    Vector.map(_ -> g(r))

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


pub def fib(n: Int32) : Int32 =
    if (n <= 1) 
        1
    else
        fib(n - 1) + fib(n - 2)

pub def slowId(n: Int32): Int32 = 
    (fib(30) / fib(30)) * n
