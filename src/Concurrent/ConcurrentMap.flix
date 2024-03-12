/*
 * Copyright 2020 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

///
/// The Mutable Map type.
///
mod Concurrent2 {

    pub enum ConcurrentMap[k: Type, v: Type, r: Region] {
        case ConcurrentMap(Region[r], Ref[Map[k, v], r], Concurrent2.ReentrantLock[r])
    }

    mod ConcurrentMap {

        use Concurrent2.ReentrantLock;

        pub def toMutMap(rc: Region[r], m: ConcurrentMap[k, v, r]): MutMap[k, v, r] \ r =
            let ConcurrentMap(_, mm, _) = m;
            MutMap.MutMap(rc, ref (deref mm) @ rc)
        
        pub def fromMutMap(rc: Region[r], m: MutMap[k, v, r]): ConcurrentMap[k, v, r] \ r =
            let MutMap.MutMap(_, mm) = m;
            ConcurrentMap(rc, ref (deref mm) @ rc, ReentrantLock.newLock(rc, false))

        def unlock(l: ReentrantLock[r]): Unit \ r =
            match ReentrantLock.unlock(l) {
                case Ok(_) => ()
                case Err(_) =>
                    // Error: unlock without holding the lock
                    bug!("Implementation error: lock not held")
            }

        ///
        /// Returns a string representation of the given ConcurrentMap `m`.
        ///
        pub def toString(m: ConcurrentMap[k, v, r]): String \ r with ToString[k], ToString[v] = region rc {
            let sb = StringBuilder.empty(rc);
            StringBuilder.appendString!("ConcurrentMap#{", sb);
            forEachWithIndex((i, k, v) -> {
                if (i < 1)
                    StringBuilder.appendString!("${k} => ${v}", sb)
                else
                    StringBuilder.appendString!(", ${k} => ${v}", sb)
                }, m);
            StringBuilder.appendString!("}", sb);
            StringBuilder.toString(sb)
        }

        ///
        /// Returns a fresh empty mutable map.
        ///
        pub def empty(rc: Region[r]): ConcurrentMap[k, v, r] \ r =
            ConcurrentMap(rc, ref Map.empty() @ rc, ReentrantLock.newLock(rc, false))

        ///
        /// Returns the singleton map where key `k` is mapped to value `v`.
        ///
        pub def singleton(rc: Region[r], k: k, v: v): ConcurrentMap[k, v, r] \ r with Order[k] =
            ConcurrentMap(rc, ref Map.singleton(k, v) @ rc, ReentrantLock.newLock(rc, false))

        ///
        /// Returns `true` if and only if the mutable map `m` contains the key `k`.
        ///
        pub def memberOf(k: k, m: ConcurrentMap[k, v, r]): Bool \ r with Order[k] =
            let ConcurrentMap(_, mm, _) = m;
            Map.memberOf(k, deref mm)

        ///
        /// Returns `true` if and only if `m` is the empty map.
        ///
        pub def isEmpty(m: ConcurrentMap[k, v, r]): Bool \ r =
            let ConcurrentMap(_, mm, _) = m;
            Map.isEmpty(deref mm)

        ///
        /// Optionally finds `k -> v` where `k` is the smallest key according to the `Order` instance of `k`.
        ///
        /// Returns `None` if `m` is empty.
        ///
        pub def minimumKey(m: ConcurrentMap[k, v, r]): Option[(k, v)] \ r =
            let ConcurrentMap(_, mm, _) = m;
            Map.minimumKey(deref mm)

        ///
        /// Optionally finds `k => v` where `k` is the smallest key according to the given comparator `cmp`.
        ///
        /// Returns `None` if `m` is empty.
        ///
        /// Purity reflective: Runs in parallel when given a pure function `cmp`.
        ///
        pub def minimumKeyBy(cmp: (k, k) -> Comparison \ ef, m: ConcurrentMap[k, v, r]): Option[(k, v)] \ { ef, r } =
            let ConcurrentMap(_, mm, _) = m;
            Map.minimumKeyBy(cmp, deref mm)

        ///
        /// Optionally finds `k => v` where `v` is the smallest value.
        ///
        /// Returns `None` if `m` is empty.
        ///
        @Parallel
        pub def minimumValue(m: ConcurrentMap[k, v, r]): Option[(k, v)] \ r with Order[v] =
            let ConcurrentMap(_, mm, _) = m;
            Map.minimumValue(deref mm)

        ///
        /// Optionally finds `k => v` where `v` is the smallest value according to the given comparator `cmp`.
        ///
        /// Returns `None` if `m` is empty.
        ///
        /// Purity reflective: Runs in parallel when given a pure function `cmp`.
        ///
        pub def minimumValueBy(cmp: (v, v) -> Comparison \ ef, m: ConcurrentMap[k, v, r]): Option[(k, v)] \ { ef, r } =
            let ConcurrentMap(_, mm, _) = m;
            Map.minimumValueBy(cmp, deref mm)

        ///
        /// Optionally finds `k => v` where `k` is the largest key according to the `Order` instance of `k`.
        ///
        /// Returns `None` if `m` is empty.
        ///
        pub def maximumKey(m: ConcurrentMap[k, v, r]): Option[(k, v)] \ r =
            let ConcurrentMap(_, mm, _) = m;
            Map.maximumKey(deref mm)

        ///
        /// Optionally finds `k => v` where `k` is the largest key according to the given comparator `cmp`.
        ///
        /// Returns `None` if `m` is empty.
        ///
        /// Purity reflective: Runs in parallel when given a pure function `f`.
        ///
        pub def maximumKeyBy(cmp: (k, k) -> Comparison \ ef, m: ConcurrentMap[k, v, r]): Option[(k, v)] \ { ef, r } =
            let ConcurrentMap(_, mm, _) = m;
            Map.maximumKeyBy(cmp, deref mm)

        ///
        /// Optionally finds `k => v` where `v` is the largest value.
        ///
        /// Returns `None` if `m` is empty.
        ///
        @Parallel
        pub def maximumValue(m: ConcurrentMap[k, v, r]): Option[(k, v)] \ r with Order[v] =
            let ConcurrentMap(_, mm, _) = m;
            Map.maximumValue(deref mm)

        ///
        /// Optionally finds `k => v` where `v` is the largest value according to the given comparator `cmp`.
        ///
        /// Returns `None` if `m` is empty.
        ///
        /// Purity reflective: Runs in parallel when given a pure function `cmp`.
        ///
        pub def maximumValueBy(cmp: (v, v) -> Comparison \ ef, m: ConcurrentMap[k, v, r]): Option[(k, v)] \ { ef, r } =
            let ConcurrentMap(_, mm, _) = m;
            Map.maximumValueBy(cmp, deref mm)

        ///
        /// Returns the keys of the mutable map `m`.
        ///
        pub def keysOf(m: ConcurrentMap[k, v, r]): Set[k] \ r with Order[k] =
            let ConcurrentMap(_, mm, _) = m;
            Map.keysOf(deref mm)

        ///
        /// Returns the values of the mutable map `m`.
        ///
        pub def valuesOf(m: ConcurrentMap[k, v, r]): List[v] \ r =
            let ConcurrentMap(_, mm, _) = m;
            Map.valuesOf(deref mm)

        ///
        /// Returns the size of the mutable map `m`.
        ///
        pub def size(m: ConcurrentMap[k, v, r]): Int32 \ r =
            let ConcurrentMap(_, mm, _) = m;
            Map.size(deref mm)

        ///
        /// Returns `Some(v)` if `k -> v` is in the mutable map `m`.
        ///
        /// Otherwise returns `None`.
        ///
        pub def get(k: k, m: ConcurrentMap[k, v, r]): Option[v] \ r with Order[k] =
            let ConcurrentMap(_, mm, _) = m;
            Map.get(k, deref mm)

        ///
        /// Returns `v` if `k -> v` is in the mutable map `m`.
        ///
        /// Otherwise returns `d`.
        ///
        pub def getWithDefault(k: k, d: v, m: ConcurrentMap[k, v, r]): v \ r with Order[k] =
            let ConcurrentMap(_, mm, _) = m;
            Map.getWithDefault(k, d, deref mm)

        ///
        /// Updates the mutable map `m` with the binding `k -> v`. Replaces any existing binding.
        ///
        pub def put!(k: k, v: v, m: ConcurrentMap[k, v, r]): Unit \ r with Order[k] =
            let ConcurrentMap(_, mm, l) = m;
            ReentrantLock.lock(l);
            mm := Map.insert(k, v, deref mm);
            unlock(l)

        ///
        /// Updates the mutable map `m` with the binding `k -> f(v, v1)` if `k -> v1` is in `m`.
        ///
        /// Otherwise updates the mutable map `m` with the binding `k -> v`.
        ///
        pub def putWith!(f: (v, v) -> v \ ef, k: k, v: v, m: ConcurrentMap[k, v, r]): Unit \ { ef, r } with Order[k] =
            let ConcurrentMap(_, mm, l) = m;
            ReentrantLock.lock(l);
            mm := Map.insertWith(f, k, v, deref mm);
            unlock(l)

        ///
        /// Updates the mutable map `m` with `k -> f(v)` if `k -> v` is in `m`.
        ///
        /// Otherwise leaves the map is unchanged.
        ///
        pub def adjust!(f: v -> v \ ef, k: k, m: ConcurrentMap[k, v, r]): Unit \ { ef, r } with Order[k] =
            adjustWithKey!((_, v1) -> f(v1), k, m)

        ///
        /// Updates the mutable map `m` with `k -> f(k, v)` if `k -> v` is in `m`.
        ///
        /// Otherwise leaves the map is unchanged.
        ///
        pub def adjustWithKey!(f: (k, v) -> v \ ef, k: k, m: ConcurrentMap[k, v, r]): Unit \ { ef, r } with Order[k] =
            let ConcurrentMap(_, mm, l) = m;
            ReentrantLock.lock(l);
            mm := Map.adjustWithKey(f, k, deref mm);
            unlock(l)

        ///
        /// Removes all mappings from the mutable map `m`.
        ///
        pub def clear!(m: ConcurrentMap[k, v, r]): Unit \ r =
            let ConcurrentMap(_, mm, l) = m;
            ReentrantLock.lock(l);
            mm := Map.empty();
            unlock(l)

        ///
        /// Returns `v` if `k -> v` is in the mutable map `m`.
        ///
        /// Otherwise updates the mutable map `m` with a new mapping `k -> d` and returns d.
        ///
        pub def getOrElsePut!(k: k, d: v, m: ConcurrentMap[k, v, r]): v \ r with Order[k] =
            let ConcurrentMap(_, mm, l) = m;
            ReentrantLock.lock(l);
            match Map.get(k, deref mm) {
                case None    =>
                    mm := Map.insert(k, d, deref mm);
                    unlock(l);
                    d
                case Some(v) =>
                    unlock(l);
                    v
            }

        // ///
        // /// Merges the mutable map `m1` into the mutable map `m2` in a left-biased manner.
        // ///
        // /// That is, key collisions are resolved by taking the mapping from `m1`.
        // ///
        // pub def merge!(m1: ConcurrentMap[k, v, r1], m2: ConcurrentMap[k, v, r2]): Unit \ { r1, r2 } with Order[k] =
        //     mergeWithKey!((_, v1, _) -> v1, m1, m2)

        // ///
        // /// Merges the mutable map `m1` into the mutable map `m2` where key collisions are resolved with the merge function `f`.
        // ///
        // pub def mergeWith!(f: (v, v) -> v \ ef, m1: ConcurrentMap[k, v, r1], m2: ConcurrentMap[k, v, r2]): Unit \ { ef, r1, r2 } with Order[k] =
        //     mergeWithKey!(_ -> f, m1, m2)

        // ///
        // /// Merges the mutable map `m1` into the mutable map `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
        // ///
        // pub def mergeWithKey!(f: (k, v, v) -> v \ ef, m1: ConcurrentMap[k, v, r1], m2: ConcurrentMap[k, v, r2]): Unit \ { ef, r1, r2 } with Order[k] =
        //     let ConcurrentMap(_, mm1) = m1;
        //     let ConcurrentMap(_, mm2) = m2;
        //     mm2 := Map.unionWithKey(f, deref mm1, deref mm2)

        // ///
        // /// Removes all mappings `k -> v` from the mutable map `m` where `v` does not satisfy the predicate function `f`.
        // ///
        // /// The function `f` must be pure.
        // ///
        // pub def refine!(f: v -> Bool, m: ConcurrentMap[k, v, r]): Unit \ r with Order[k] =
        //     refineWithKey!(_ -> f, m)

        // ///
        // /// Removes all mappings `k -> v` from the mutable map `m` where `(k, v)` does not satisfy the predicate function `f`.
        // ///
        // /// The function `f` must be pure.
        // ///
        // pub def refineWithKey!(f: (k, v) -> Bool, m: ConcurrentMap[k, v, r]): Unit \ r with Order[k] =
        //     let ConcurrentMap(_, mm) = m;
        //     mm := Map.filterWithKey(f, deref mm)

        ///
        /// Removes the mapping `k` from the mutable map `m`.
        ///
        /// Leaves the map unchanged if the mutable map `m` does not contain any mapping for `k`.
        ///
        pub def remove!(k: k, m: ConcurrentMap[k, v, r]): Unit \ r with Order[k] =
            let ConcurrentMap(_, mm, l) = m;
            ReentrantLock.lock(l);
            mm := Map.remove(k, deref mm);
            unlock(l)

        // ///
        // /// Applies the function `f` to every value in the mutable map `m`.
        // ///
        // pub def transform!(f: v -> v \ ef, m: ConcurrentMap[k, v, r]): Unit \ { ef, r } with Order[k] =
        //     let ConcurrentMap(_, mm) = m;
        //     mm := Map.map(f, deref mm)

        // ///
        // /// Applies the function `f` to every value in the mutable map `m`.
        // ///
        // pub def transformWithKey!(f: (k, v) -> v \ ef, m: ConcurrentMap[k, v, r]): Unit \ { ef, r } with Order[k] =
        //     let ConcurrentMap(_, mm) = m;
        //     mm := Map.mapWithKey(f, deref mm)

        // ///
        // /// Returns `true` if and only if all mappings in the mutable map `m1` occur in the mutable map `m2`.
        // ///
        // pub def isSubmapOf(m1: ConcurrentMap[k, v, r1], m2: ConcurrentMap[k, v, r2]): Bool \ { r1, r2 } with Order[k], Eq[v] =
        //     let ConcurrentMap(_, mm1) = m1;
        //     let ConcurrentMap(_, mm2) = m2;
        //     Map.isSubmapOf(deref mm1, deref mm2)

        // ///
        // /// Returns `true` if and only if all mappings in the mutable map `m1` occur in the mutable map `m2` and `m1 != m2`.
        // ///
        // pub def isProperSubmapOf(m1: ConcurrentMap[k, v, r1], m2: ConcurrentMap[k, v, r2]): Bool \ { r1, r2 } with Order[k], Eq[v] =
        //     let ConcurrentMap(_, mm1) = m1;
        //     let ConcurrentMap(_, mm2) = m2;
        //     Map.isProperSubmapOf(deref mm1, deref mm2)

        // ///
        // /// Alias for `findLeft`.
        // ///
        // /// The function `f` must be pure.
        // ///
        // pub def find(f: (k, v) -> Bool, m: ConcurrentMap[k, v, r]): Option[(k, v)] \ r =
        //     findLeft(f, m)

        // ///
        // /// Optionally returns the first mapping of the mutable map `m` that satisfies the predicate function `f` when searching from left to right.
        // ///
        // /// The function `f` must be pure.
        // ///
        // pub def findLeft(f: (k, v) -> Bool, m: ConcurrentMap[k, v, r]): Option[(k, v)] \ r =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.findLeft(f, deref mm)

        // ///
        // /// Optionally returns the first mapping of the mutable map `m` that satisfies the predicate function `f` when searching from right to left.
        // ///
        // /// The function `f` must be pure.
        // ///
        // pub def findRight(f: (k, v) -> Bool, m: ConcurrentMap[k, v, r]): Option[(k, v)] \ r =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.findRight(f, deref mm)

        // ///
        // /// Returns a map with mappings `k => f(v)` for every `k => v` in `m`.
        // ///
        // /// Purity reflective: Runs in parallel when given a pure function `f`.
        // ///
        // pub def map(rc1: Region[r1], f: v1 -> v2 \ ef, m: ConcurrentMap[k, v1, r]): ConcurrentMap[k, v2, r1] \ { ef, r, r1 } =
        //     mapWithKey(rc1, _ -> f, m)

        // ///
        // /// Returns a map with mappings `k => f(k, v)` for every `k => v` in `m`.
        // ///
        // /// Purity reflective: Runs in parallel when given a pure function `f`.
        // ///
        // pub def mapWithKey(rc: Region[r1], f: (k, v1) -> v2 \ ef, m: ConcurrentMap[k, v1, r]): ConcurrentMap[k, v2, r1] \ { ef, r, r1 } =
        //     let ConcurrentMap(_, mm) = m;
        //     ConcurrentMap(rc, ref Map.mapWithKey(f, deref mm) @ rc)

        // ///
        // /// Alias for `foldLeftWithKey`.
        // ///
        // pub def foldWithKey(f: (b, k, v) -> b \ ef, i: b, m: ConcurrentMap[k, v, r]): b \ { ef, r }  =
        //     foldLeftWithKey(f, i, m)

        // ///
        // /// Applies `f` to a start value `i` and all values in the mutable map `m` going from left to right.
        // ///
        // /// That is, the result is of the form: `f(...f(f(i, v1), v2)..., vn)`.
        // ///
        // pub def foldLeft(f: (b, v) -> b \ ef, i: b, m: ConcurrentMap[k, v, r]): b \ { ef, r } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.foldLeft(f, i, deref mm)

        // ///
        // /// Applies `f` to a start value `i` and all key-value pairs in the mutable map `m` going from left to right.
        // ///
        // /// That is, the result is of the form: `f(...f(k2, f(k1, i, v1), v2)..., vn)`.
        // ///
        // pub def foldLeftWithKey(f: (b, k, v) -> b \ ef, i: b, m: ConcurrentMap[k, v, r]): b \ { ef, r } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.foldLeftWithKey(f, i, deref mm)

        // ///
        // /// Applies `f` to a start value `s` and all values in the mutable map `m` going from right to left.
        // ///
        // /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
        // ///
        // pub def foldRight(f: (v, b) -> b \ ef, s: b, m: ConcurrentMap[k, v, r]): b \ { ef, r } =
        //     foldRightWithKey((_, v, b) -> f(v, b), s, m)

        // ///
        // /// Applies `f` to a start value `s` and all key-value pairs in the mutable map `m` going from right to left.
        // ///
        // /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
        // ///
        // pub def foldRightWithKey(f: (k, v, b) -> b \ ef, s: b, m: ConcurrentMap[k, v, r]): b \ { ef, r } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.foldRightWithKey(f, s, deref mm)

        // ///
        // /// Applies `f` to a start value `z` and all values in the mutable map `m` going from right to left.
        // ///
        // /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, z)))`.
        // /// A `foldRightWithCont` allows early termination by not calling the continuation.
        // ///
        // pub def foldRightWithCont(f: (v, Unit -> b \ ef) -> b \ ef, z: b, m: ConcurrentMap[k, v, r]): b \ { ef, r } =
        //     foldRightWithKeyCont((_, v, b) -> f(v, b), z, m)

        // ///
        // /// Applies `f` to a start value `z` and all key-value pairs in the mutable map `m` going from right to left.
        // ///
        // /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, z)))`.
        // /// A `foldRightWithKeyCont` allows early termination by not calling the continuation.
        // ///
        // pub def foldRightWithKeyCont(f: (k, v, Unit -> b \ ef) -> b \ ef, z: b, m: ConcurrentMap[k, v, r]): b \ { ef, r } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.foldRightWithKeyCont(f, z, deref mm)

        // ///
        // /// Applies `f` to all values in the mutable map `m` going from left to right until a single value `v` is obtained. Returns `Some(v)`.
        // ///
        // /// That is, the result is of the form: `Some(f(...f(f(v1, v2), v3)..., vn))`
        // ///
        // /// Returns `None` if `m` is the empty map.
        // ///
        // pub def reduceLeft(f: (v, v) -> v \ ef, m: ConcurrentMap[k, v, r]): Option[v] \ { ef, r } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.reduceLeft(f, deref mm)

        // ///
        // /// Applies `f` to all mappings in the mutable map `m` going from left to right until a single mapping `(k, v)` is obtained. Returns `Some((k, v))`.
        // ///
        // /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
        // ///
        // /// Returns `None` if `m` is the empty map.
        // ///
        // pub def reduceLeftWithKey(f: (k, v, k, v) -> (k, v) \ ef, m: ConcurrentMap[k, v, r]): Option[(k, v)] \ { ef, r } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.reduceLeftWithKey(f, deref mm)

        // ///
        // /// Applies `f` to all values in the mutable map `m` going from right to left until a single value `v` is obtained. Returns `Some(v)`.
        // ///
        // /// That is, the result is of the form: `Some(f(v1, ...f(vn-2, f(vn-1, vn))...))`
        // ///
        // /// Returns `None` if `m` is the empty map.
        // ///
        // pub def reduceRight(f: (v, v) -> v \ ef, m: ConcurrentMap[k, v, r]): Option[v] \ { ef, r } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.reduceRight(f, deref mm)

        // ///
        // /// Applies `f` to all mappings in the mutable map `m` going from right to left until a single mapping `(k, v)` is obtained. Returns `Some((k, v))`.
        // ///
        // /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
        // ///
        // /// Returns `None` if `m` is the empty map.
        // ///
        // pub def reduceRightWithKey(f: (k, v, k, v) -> (k, v) \ ef, m: ConcurrentMap[k, v, r]): Option[(k, v)] \ { ef, r } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.reduceRightWithKey(f, deref mm)

        // ///
        // /// Returns the number of mappings in the mutable map `m` that satisfy the predicate function `f`.
        // ///
        // /// Purity reflective: Runs in parallel when given a pure function `f`.
        // ///
        // pub def count(f: (k, v) -> Bool \ ef, m: ConcurrentMap[k, v, r]): Int32 \ { ef, r } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.count(f, deref mm)

        // ///
        // /// Returns the sum of all keys in the map `m`.
        // ///
        // pub def sumKeys(m: ConcurrentMap[Int32, v, r]): Int32 \ r =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.sumKeys(deref mm)

        // ///
        // /// Returns the sum of all values in the map `m`.
        // ///
        // pub def sumValues(m: ConcurrentMap[k, Int32, r]): Int32 \ r =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.sumValues(deref mm)

        // ///
        // /// Returns the sum of all key-value pairs `k => v` in the map `m` according to the function `f`.
        // ///
        // /// Purity reflective: Runs in parallel when given a pure function `f`.
        // ///
        // pub def sumWith(f: (k, v) -> Int32 \ ef, m: ConcurrentMap[k, v, r]): Int32 \ { ef, r } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.sumWith(f, deref mm)

        // ///
        // /// Returns `true` if and only if at least one mapping in the mutable map `m` satisfies the predicate function `f`.
        // ///
        // /// Returns `false` if `m` is the empty map.
        // ///
        // pub def exists(f: (k, v) -> Bool \ ef, m: ConcurrentMap[k, v, r]): Bool \ { ef, r } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.exists(f, deref mm)

        // ///
        // /// Returns `true` if and only if all mappings in the mutable map `m` satisfy the predicate function `f`.
        // ///
        // /// Returns `true` if `m` is the empty map.
        // ///
        // pub def forAll(f: (k, v) -> Bool \ ef, m: ConcurrentMap[k, v, r]): Bool \ { ef, r } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.forAll(f, deref mm)

        // ///
        // /// Returns a shallow copy of the mutable map `m`.
        // ///
        // pub def copy(rc: Region[r1], m: ConcurrentMap[k, v, r]): ConcurrentMap[k, v, r1] \ { r, r1 } =
        //     let ConcurrentMap(_, mm) = m;
        //     ConcurrentMap(rc, ref (deref mm) @ rc)

        // ///
        // /// Returns the mutable map `m` as an immutable map.
        // ///
        // pub def toMap(m: ConcurrentMap[k, v, r]): Map[k, v] \ r =
        //     let ConcurrentMap(_, mm) = m;
        //     deref mm

        // ///
        // /// Returns `m` as a MutDeque.
        // ///
        // pub def toMutDeque(rc1: Region[r1], m: ConcurrentMap[k, v, r2]): MutDeque[(k, v), r1] \ { r2, r1 } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.toMutDeque(rc1, deref mm)

        // ///
        // /// Returns the mutable map `m` as a list of key-value pairs.
        // ///
        // pub def toList(m: ConcurrentMap[k, v, r]): List[(k, v)] \ r =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.toList(deref mm)

        // ///
        // /// Returns the mutable map `m` as a set of key-value pairs.
        // ///
        // pub def toSet(m: ConcurrentMap[k, v, r]): Set[(k, v)] \ r with Order[k], Order[v] =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.toSet(deref mm)

        ///
        /// Applies `f` to all the `(key, value)` pairs in the mutable map `m`.
        ///
        pub def forEach(f: (k, v) -> Unit \ ef, m: ConcurrentMap[k, v, r]): Unit \ { ef, r } =
            let ConcurrentMap(_, mm, _) = m;
            Map.forEach(f, deref mm)

        ///
        /// Apply the effectful function `f` to all the `(key, value)` pairs in the mutable map `m`
        /// along with that element's index.
        ///
        pub def forEachWithIndex(f: (Int32, k, v) -> Unit \ ef, m: ConcurrentMap[k, v, r]): Unit \ { ef, r } = region rc {
            let ix = ref 0 @ rc;
            forEach((k, v) -> {let i = deref ix; f(i, k, v); ix := i+1}, m)
        }

        // ///
        // /// Returns an iterator over all key-value pairs in `m`.
        // ///
        // pub def iterator(rc: Region[r1], m: ConcurrentMap[k, v, r2]): Iterator[(k, v), r1 + r2, r1] \ { r1, r2 } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.iterator(rc, deref mm) |> Iterator.map(x -> {discard deref mm; x})

        // ///
        // /// Returns an iterator over keys in `m`.
        // ///
        // pub def iteratorKeys(rc: Region[r1], m: ConcurrentMap[k, v, r2]): Iterator[k, r1 + r2, r1] \ { r1, r2 } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.iteratorKeys(rc, deref mm) |> Iterator.map(x -> {discard deref mm; x})

        // ///
        // /// Returns an iterator over values in `m`.
        // ///
        // pub def iteratorValues(rc: Region[r1], m: ConcurrentMap[k, v, r2]): Iterator[v, r1 + r2, r1] \ { r1, r2 } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.iteratorValues(rc, deref mm) |> Iterator.map(x -> {discard deref mm; x})

        ///
        /// Extracts a range of key-value pairs from the mutable map `m`.
        ///
        /// That is, the result is a list of all pairs `(k, v)` where `p(k)` returns `Equal`.
        ///
        pub def rangeQuery(p: k -> Comparison \ ef, m: ConcurrentMap[k, v, r]): List[(k, v)] \ { ef, r } =
            let ConcurrentMap(_, mm, _) = m;
            Map.rangeQuery(p, deref mm)

        ///
        /// Applies `f` to all key-value pairs `(k, v)` in the mutable map `m` where `p(k)` returns `EqualTo`.
        ///
        pub def rangeQueryWith(p: k -> Comparison \ ef1, f: (k, v) -> Unit \ ef2, m: ConcurrentMap[k, v, r]): Unit \ { ef1, ef2, r } =
            let ConcurrentMap(_, mm, _) = m;
            Map.rangeQueryWith(p, f, deref mm)

        // ///
        // /// Returns `true` if ConcurrentMaps `a` and `b` have the same elements, i.e. are structurally equal.
        // ///
        // pub def sameElements(a: ConcurrentMap[k, v, r1], b: ConcurrentMap[k, v, r2]): Bool \ { r1, r2 } with Order[k], Eq[v] =
        //     (ConcurrentMap.isSubmapOf(a, b) and ConcurrentMap.isSubmapOf(b, a))

        // ///
        // /// Returns the concatenation of the string representation of each key `k`
        // /// in `m` with `sep` inserted between each element.
        // ///
        // pub def joinKeys(sep: String, m: ConcurrentMap[k, v, r]): String \ r with ToString[k] =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.joinKeys(sep, deref mm)

        // ///
        // /// Returns the concatenation of the string representation of each value `v`
        // /// in `m` with `sep` inserted between each element.
        // ///
        // pub def joinValues(sep: String, m: ConcurrentMap[k, v, r]): String \ r with ToString[v] =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.joinValues(sep, deref mm)

        // ///
        // /// Returns the concatenation of the string representation of each key-value pair
        // /// `k => v` in `m` according to `f` with `sep` inserted between each element.
        // ///
        // pub def joinWith(f: (k, v) -> String \ ef, sep: String, m: ConcurrentMap[k, v, r]): String \ { ef, r } =
        //     let ConcurrentMap(_, mm) = m;
        //     Map.joinWith(f, sep, deref mm)

    }
}
