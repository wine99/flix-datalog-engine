mod Benchmark.Fixpoint.Pointer {
    @benchmark
    pub def pointsTo(): Int32 \ IO =
        let numInstructions = 1000;
        let numVariables = 2000;

        let p = #{
            PointsTo(x, y) :- AddressOf(x, y).
            PointsTo(x, y) :- Assign(x, z), PointsTo(z, y).
            PointsTo(x, w) :- Load(x, z), PointsTo(z, y), PointsTo(y, w).
            PointsTo(x, y) :- Store(z, w), PointsTo(z, x), PointsTo(w, y).
        };

        let addressOf = inject genFacts(numInstructions, genFact2(numVariables), None) into AddressOf;
        let assign = inject genFacts(numInstructions, genFact2(numVariables), None) into Assign;
        let load = inject genFacts(numInstructions, genFact2(numVariables), None) into Load;
        let store = inject genFacts(numInstructions, genFact2(numVariables), None) into Store;
        let pointsTo = query p, addressOf, assign, load, store select (x, y) from PointsTo(x, y);
        Vector.length(pointsTo)

    pub def pointsTo2(): Int32 \ IO =
        let numInstructions = 1000;
        let numVariables = 2000;

        let p = #{
            PointsTo(x, y) :- AddressOf(x, y).
            PointsTo(x, y) :- Assign(x, z), PointsTo(z, y).
            // This is one is much slower since we cannot use `query` on PointsTo(y, w) but only `search`
            // We can try to make the compiler reorder relations
            PointsTo(x, w) :- Load(x, z), PointsTo(y, w), PointsTo(z, y).
            PointsTo(x, y) :- Store(z, w), PointsTo(z, x), PointsTo(w, y).
        };

        let addressOf = inject genFacts(numInstructions, genFact2(numVariables), None) into AddressOf;
        let assign = inject genFacts(numInstructions, genFact2(numVariables), None) into Assign;
        let load = inject genFacts(numInstructions, genFact2(numVariables), None) into Load;
        let store = inject genFacts(numInstructions, genFact2(numVariables), None) into Store;
        let pointsTo = query p, addressOf, assign, load, store select (x, y) from PointsTo(x, y);
        Vector.length(pointsTo)

    pub def pointsTo3(): Int32 \ IO =
        let numInstructions = 1000;
        let numVariables = 2000;

        let p = #{
            PointsTo(x, y) :- AddressOf(x, y).
            PointsTo(x, y) :- Assign(x, z), PointsTo(z, y).
            // This is one is much slower since we cannot use `query` on PointsTo(y, w) but only `search`
            // We can try to make the compiler reorder relations
            PointsTo(x, w) :- Load(x, z), PointsTo(y, w), PointsTo(z, y).
            PointsTo(x, y) :- Store(z, w), PointsTo(z, x), PointsTo(w, y).
        };

        let addressOf = inject genFacts(numInstructions, genFact2(numVariables), None) into AddressOf;
        let assign = inject genFacts(numInstructions, genFact2(numVariables), None) into Assign;
        let load = inject genFacts(numInstructions, genFact2(numVariables), None) into Load;
        let store = inject genFacts(numInstructions, genFact2(numVariables), None) into Store;
        let pointsTo = query p, addressOf, assign, load, store select (x, y) from PointsTo(x, y);
        Vector.length(pointsTo)
}
