mod Benchmark.Fixpoint.Propagate {

    @benchmark
    pub def propagate1(): Int32 =
        let p = #{
            A(b+c) :- B(a, c), C(a, b), if b+c < 50.
            C(a, b) :- A(b), C(a, _).
            C(a, b) :- A(a), C(_, b).
            B(a, b) :- A(b), B(a, _).
            B(a, b) :- A(a), B(_, b).
        };
        let f = #{
            B(1, 1).
            C(1, 1).
        };
        query p <+> f select x from A(x)
        |> Vector.length

    @benchmark
    pub def propagate2(): Int32 =
        let p = #{
            A(a+b) :- B(a, c), C(b, c), if a+b < 50.
            C(a, b) :- A(b), C(a, _).
            C(a, b) :- A(a), C(_, b).
            B(a, b) :- A(b), B(a, _).
            B(a, b) :- A(a), B(_, b).
        };
        let f = #{
            B(1, 1).
            C(1, 1).
        };
        query p <+> f select x from A(x)
        |> Vector.length
}
