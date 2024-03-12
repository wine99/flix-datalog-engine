mod Benchmark.Fixpoint.Propagate {

    // @benchmark
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
        query p, f select x from A(x)
        |> Vector.length

    // @benchmark
    pub def propagate2(): Int32 =
        let p = #{
            A(a+b) :- B(a, c), C(b, c), if a+b < 50.
            B(a, b) :- A(b), B(a, _).
            B(a, b) :- A(a), B(_, b).
            C(a, b) :- A(b), C(a, _).
            C(a, b) :- A(a), C(_, b).
        };
        let f = #{
            B(1, 1).
            C(1, 1).
        };
        query p, f select x from A(x)
        |> Vector.length

    @benchmark
    pub def propaget3(): Int32 =
        let p = #{
            A(slowId(x) + 1) :- Z(x), A(x).
            B(slowId(x) + 1) :- Z(x), B(x).
            C(slowId(x) + 1) :- Z(x), C(x).
            D(slowId(x) + 1) :- Z(x), D(x).
            E(slowId(x) + 1) :- Z(x), E(x).
            F(slowId(x) + 1) :- Z(x), F(x).
            G(slowId(x) + 1) :- Z(x), G(x).
            H(slowId(x) + 1) :- Z(x), H(x).
            I(slowId(x) + 1) :- Z(x), I(x).
            J(slowId(x) + 1) :- Z(x), J(x).
            K(slowId(x) + 1) :- Z(x), K(x).
            L(slowId(x) + 1) :- Z(x), L(x).
            M(slowId(x) + 1) :- Z(x), M(x).
            N(slowId(x) + 1) :- Z(x), N(x).
            O(slowId(x) + 1) :- Z(x), O(x).
            Z(slowId(x) + 1) :- Z(x), if x < 5.
        };
        let f = #{
            Z(0).
            A(0).
            B(0).
            E(0).
            F(0).
            G(0).
            H(0).
            I(0).
            J(0).
            K(0).
            L(0).
            M(0).
            N(0).
            O(0).
        }; 
        query p, f select x from A(x)
        |> Vector.length
}
