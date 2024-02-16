mod Benchmark.Fixpoint.Intersect {
    def numRules(): Int32 = 1000
    def upperBound(): Int32 = 5000

    @benchmark
    pub def intersect1(): Int32 \ IO = {
        let upperBound = upperBound();
        let numRules = numRules();
        let p = #{
            A(1 , i) :- Check(a, b, c, d, e), In(a, b, c, d, e, i).
            A(2 , i) :- Check(a, _, c, d, e), In(a, _, c, d, e, i).
            A(3 , i) :- Check(a, _, _, d, e), In(a, _, _, d, e, i).
            A(4 , i) :- Check(a, _, _, _, e), In(a, _, _, _, e, i).
            A(5 , i) :- Check(a, _, _, _, _), In(a, _, _, _, _, i).
            A(6 , i) :- Check(a, b, _, d, e), In(a, b, _, d, e, i).
            A(7 , i) :- Check(a, b, _, _, e), In(a, b, _, _, e, i).
            A(8 , i) :- Check(a, b, _, _, _), In(a, b, _, _, _, i).
            A(9 , i) :- Check(a, b, c, _, e), In(a, b, c, _, e, i).
            A(10, i) :- Check(a, b, c, _, _), In(a, b, c, _, _, i).
            A(11, i) :- Check(a, b, c, d, _), In(a, b, c, d, _, i).
        };
        let genCheck = genFact5(upperBound);
        let genIn = genFact6(upperBound);
        let factsCheck = inject genFacts(numRules, genCheck, None) into Check;
        let factIn = inject genFacts(numRules, genIn, None) into In;
        query p, factsCheck, factIn select (x, y) from A(x, y)
        |> Vector.length
    }

    @benchmark
    pub def intersect2(): Int32 \ IO = {
        let upperBound = upperBound();
        let numRules = numRules();
        let p = #{
            A(1 , i) :- Check(a, b, c, d, e), In(a, b, c, d, e, i).
            A(2 , i) :- Check(a, b, c, _, e), In(a, b, c, _, e, i).
            A(3 , i) :- Check(a, b, _, _, e), In(a, b, _, _, e, i).
            A(4 , i) :- Check(a, _, _, _, e), In(a, _, _, _, e, i).
            A(5 , i) :- Check(_, _, _, _, e), In(_, _, _, _, e, i).
            A(6 , i) :- Check(a, b, _, d, e), In(a, b, _, d, e, i).
            A(7 , i) :- Check(a, _, _, d, e), In(a, _, _, d, e, i).
            A(8 , i) :- Check(_, _, _, d, e), In(_, _, _, d, e, i).
            A(9 , i) :- Check(a, _, c, d, e), In(a, _, c, d, e, i).
            A(10, i) :- Check(_, _, c, d, e), In(_, _, c, d, e, i).
            A(11, i) :- Check(_, b, c, d, e), In(_, b, c, d, e, i).
        };
        let genCheck = genFact5(upperBound);
        let genIn = genFact6(upperBound);
        let factsCheck = inject genFacts(numRules, genCheck, None) into Check;
        let factIn = inject genFacts(numRules, genIn, None) into In;
        query p, factsCheck, factIn select (x, y) from A(x, y)
        |> Vector.length
    }

    pub def intersect3(): Int32 \ IO = {
        let upperBound = upperBound();
        let numRules = numRules();
        let p = #{
            A(1 , i) :- Check(a, b, c, d, e), In(i, a, b, c, d, e).
            A(2 , i) :- Check(a, _, c, d, e), In(i, a, _, c, d, e).
            A(3 , i) :- Check(a, _, _, d, e), In(i, a, _, _, d, e).
            A(4 , i) :- Check(a, _, _, _, e), In(i, a, _, _, _, e).
            A(5 , i) :- Check(a, _, _, _, _), In(i, a, _, _, _, _).
            A(6 , i) :- Check(a, b, _, d, e), In(i, a, b, _, d, e).
            A(7 , i) :- Check(a, b, _, _, e), In(i, a, b, _, _, e).
            A(8 , i) :- Check(a, b, _, _, _), In(i, a, b, _, _, _).
            A(9 , i) :- Check(a, b, c, _, e), In(i, a, b, c, _, e).
            A(10, i) :- Check(a, b, c, _, _), In(i, a, b, c, _, _).
            A(11, i) :- Check(a, b, c, d, _), In(i, a, b, c, d, _).
        };
        let genCheck = genFact5(upperBound);
        let genIn = genFact6(upperBound);
        let factsCheck = inject genFacts(numRules, genCheck, None) into Check;
        let factIn = inject genFacts(numRules, genIn, None) into In;
        query p, factsCheck, factIn select (x, y) from A(x, y)
        |> Vector.length
    }

    pub def intersect4(): Int32 \ IO = {
        let upperBound = upperBound();
        let numRules = numRules();
        let p = #{
            A(1 , i) :- Check(a, b, c, d, e), In(i, a, b, c, d, e).
            A(2 , i) :- Check(a, b, c, _, e), In(i, a, b, c, _, e).
            A(3 , i) :- Check(a, b, _, _, e), In(i, a, b, _, _, e).
            A(4 , i) :- Check(a, _, _, _, e), In(i, a, _, _, _, e).
            A(5 , i) :- Check(_, _, _, _, e), In(i, _, _, _, _, e).
            A(6 , i) :- Check(a, b, _, d, e), In(i, a, b, _, d, e).
            A(7 , i) :- Check(a, _, _, d, e), In(i, a, _, _, d, e).
            A(8 , i) :- Check(_, _, _, d, e), In(i, _, _, _, d, e).
            A(9 , i) :- Check(a, _, c, d, e), In(i, a, _, c, d, e).
            A(10, i) :- Check(_, _, c, d, e), In(i, _, _, c, d, e).
            A(11, i) :- Check(_, b, c, d, e), In(i, _, b, c, d, e).
        };
        let genCheck = genFact5(upperBound);
        let genIn = genFact6(upperBound);
        let factsCheck = inject genFacts(numRules, genCheck, None) into Check;
        let factIn = inject genFacts(numRules, genIn, None) into In;
        query p, factsCheck, factIn select (x, y) from A(x, y)
        |> Vector.length
    }
}
