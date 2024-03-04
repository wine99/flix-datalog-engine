mod Benchmark.Fixpoint.Parallel {
    def genWithSeed(_seed: Int64): List[Int32] \ IO =
        // let upperBound = 1_000_000;
        // let num = 1_000_00;
        // genFacts(num, genFact1(upperBound), Some(seed))
        unchecked_cast(List.range(0, 1_000_00) as List[Int32] \ IO)

    @benchmark
    pub def parallel1(): Int32 \ IO = {
        let p = #{
            R(x) :- B(x).
            R1(x) :- A(x), R(x).
            R2(x) :- A(x), R(x).
            R3(x) :- A(x), R(x).
            R4(x) :- A(x), R(x).
        };
        let a = inject genWithSeed(0i64) into A;
        let b = inject genWithSeed(1i64) into B;
        query p, a, b select x from R1(x) |> Vector.length
    }

    @benchmark
    pub def parallel2(): Int32 \ IO = {
        let p = #{
            R(x) :- B(x).
            R1(x) :- A(x), R(x).
            R2(x) :- A(x), R(x).
            R3(x) :- A(x), R(x).
            R4(x) :- A(x), R(x).
            R5(x) :- A(x), R(x).
            R6(x) :- A(x), R(x).
            R7(x) :- A(x), R(x).
            R8(x) :- A(x), R(x).
            R9(x) :- A(x), R(x).
            R10(x) :- A(x), R(x).
            R11(x) :- A(x), R(x).
            R12(x) :- A(x), R(x).
            R13(x) :- A(x), R(x).
            R14(x) :- A(x), R(x).
            R15(x) :- A(x), R(x).
            R16(x) :- A(x), R(x).
            R17(x) :- A(x), R(x).
            R18(x) :- A(x), R(x).
            R19(x) :- A(x), R(x).
            R20(x) :- A(x), R(x).
        };
        let a = inject genWithSeed(0i64) into A;
        let b = inject genWithSeed(1i64) into B;
        query p, a, b select x from R1(x) |> Vector.length
    }
}