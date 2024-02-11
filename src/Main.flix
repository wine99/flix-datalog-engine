mod BenchmarkConstraint {

    ///
    /// create a set of facts.
    ///
    @benchmark
    pub def benchmark01(): Bool =
        let fold = (acc: #{ DirectedEdge(Int32, Int32) }, s: Int32) -> {
            let d = s + 1;
            acc <+> #{ DirectedEdge(s, d). }
        };
        let facts = List.range(1, 20) |> List.foldLeft(fold, #{});
        query facts select true from DirectedEdge(1, 2) |> Vector.isEmpty |> Bool.not

    ///
    /// create a set of facts and compute the symmetric facts.
    ///
    @benchmark
    pub def benchmark02(): Bool =
        let fold = (acc: #{ DirectedEdge(Int32, Int32) }, s: Int32) -> {
            let d = s + 1;
            acc <+> #{ DirectedEdge(s, d). }
        };
        let facts = List.range(1, 20) |> List.foldLeft(fold, #{});
        let rules = #{
            DirectedEdge(y, x) :- DirectedEdge(x, y).
        };
        query facts <+> rules select true from DirectedEdge(1, 2) |> Vector.isEmpty |> Bool.not

    ///
    /// create a set of facts and compute the transitive closure.
    ///
    @benchmark
    pub def benchmark03(): Bool =
        let fold = (acc: #{ DirectedEdge(Int32, Int32), DirectedPath(Int32, Int32) }, s: Int32) -> {
            let d = s + 1;
            acc <+> #{ DirectedEdge(s, d). }
        };
        let facts = List.range(1, 20) |> List.foldLeft(fold, #{});
        let rules = #{
            DirectedPath(x, y) :- DirectedEdge(x, y).
            DirectedPath(x, z) :- DirectedPath(x, y), DirectedEdge(y, z).
        };
        query facts <+> rules select true from DirectedPath(1, 20) |> Vector.isEmpty |> Bool.not

}

def isConnected(s: Set[(Int32, Int32)], src: Int32, dst: Int32): Bool =
    let rules = #{
        Path(x, y) :- Edge(x, y).
        Path(x, z) :- Path(x, y), Edge(y, z).
    };
    let edges = inject s into Edge;
    let paths = query edges, rules select true from Path(src, dst);
    not (paths |> Vector.isEmpty)

def example(): Unit \ IO =
    let s = Set#{(1, 2), (2, 3), (3, 4), (4, 5)};
    let src = 1;
    let dst = 5;
    if (isConnected(s, src, dst)) {
        println("Found a path between ${src} and ${dst}!")
    } else {
        println("Did not find a path between ${src} and ${dst}!")
    }


def main(): Unit \ IO =
    println(BenchmarkConstraint.benchmark02())