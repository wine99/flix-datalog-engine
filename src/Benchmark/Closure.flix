mod Benchmark.Fixpoint.Closure {
    def genEdges(numNodes: Int32, numEdges: Int32): List[(Int32, Int32)] \ IO =
        genFacts(numEdges, genFact2(numNodes), None)

    def numEdges(): Int32 = 200

    def numNodes(): Int32 = 200

    def genEdgesDefault(): List[(Int32, Int32)] \ IO =
        genEdges(numNodes(), numEdges())

    @benchmark
    pub def closure1(): Int32 \ IO =
        let p = #{
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Edge(y, z).
        };
        let edges = inject genEdgesDefault() into Edge;
        query edges, p select (x, y) from Path(x, y)
        |> Vector.length

    @benchmark
    pub def closure2(): Int32 \ IO =
        let p = #{
            Edge(y, x) :- Edge(x, y).
            Path(x, y) :- Edge(x, y).
            Path(x, z) :- Path(x, y), Path(y, z).
        };
        let edges = inject genEdgesDefault() into Edge;
        query edges, p select (x, y) from Path(x, y)
        |> Vector.length

    @benchmark
    pub def componentRep(): Int32 \ IO =
        let p = #{
            Reachable(x, x) :- Edge(x, _).
            Reachable(y, y) :- Edge(_, y).
            Reachable(x, y) :- Edge(x, y).
            Reachable(y, x) :- Edge(x, y).
            Reachable(x, z) :- Reachable(x, y), Reachable(y, z).
            ReachUp(x) :- fix Reachable(x, y), if x < y.
            ComponentRep(x, rep) :- Reachable(x, rep), not ReachUp(rep).
        };
        let edges = inject genEdges(200, 150) into Edge;
        query edges, p select rep from ComponentRep(_, rep)
        |> Vector.length

    @benchmark
    pub def satellite(): Int32 \ IO =
        let p = #{
            Orbits(x, y) :- Orbits(x, z), Orbits(z, y).
            Intermediate(x, y) :- Orbits(x, z), Orbits(z, y).
            Satellite(x, y) :- Orbits(x, y), not Intermediate(x, y).
        };
        let orbits = inject genEdges(1000, 1000) into Orbits;
        query orbits, p select x from Satellite(x, _)
        |> Vector.length
}