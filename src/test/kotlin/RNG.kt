import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers
import org.junit.jupiter.api.Test

interface RNG {
    fun nextInt(): Pair<Int, RNG>
}


typealias Rand<A> = (RNG) -> Pair<A, RNG>

val intR: Rand<Int> = { rng -> rng.nextInt() }


fun <A> unit(a: A): Rand<A> = { rng -> a to rng }

fun <A, B> mapR(s: Rand<A>, f: (A) -> B): Rand<B> = { rng ->
    val (v, rng2) = s(rng)
    Pair(f(v), rng2)
}


fun innns(count: Int, rng: RNG): Pair<List<Int>, RNG> {
    if (count == 0) {
        return emptyList<Int>() to rng
    } else {
        val (value, newState) = rng.nextInt()
        val (ints, otherState) = innns(count - 1, newState)
        return listOf(value) + ints to otherState
    }
}


fun nonNegativeEven(): Rand<Int> =
    mapR(::nonNegativeInt) { it - (it % 2) }

fun nonNegativeOdd(): Rand<Int> =
    mapR(::nonNegativeInt) { it - (it % 2) + 1 }

data class SimpleRNG(val seed: Long) : RNG {
    override fun nextInt(): Pair<Int, RNG> {
        val newSeed =
            (seed * 0x5DEECE66DL + 0xBL) and
                    0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed ushr 16).toInt()
        return n to nextRNG
    }
}

fun nonNegativeInt(rng: RNG): Pair<Int, RNG> {
    val (i1, rng2) = rng.nextInt()
    return Pair(if (i1 < 0) -(i1 + 1) else i1, rng2)
}

fun nextBoolean(rng: RNG): Pair<Boolean, RNG> {
    val (i, rng2) = rng.nextInt()
    return Pair(i % 2 == 0, rng2)
}

fun double(rng: RNG): Pair<Double, RNG> {
    val (i, rng2) = nonNegativeInt(rng)
    return Pair(i / (Int.MAX_VALUE.toDouble() + 1), rng2)
}


fun double2(rng: RNG): Rand<Double> = mapR({ rng -> nonNegativeInt(rng) }, { i -> i / (Int.MAX_VALUE.toDouble() + 1) })

fun intDouble(rng: RNG): Pair<Pair<Int, Double>, RNG> {
    val (i, rng1) = rng.nextInt()
    val (d, rng2) = double(rng1)
    return Pair(Pair(i, d), rng2)
}

fun <A, B, C> map2(
    ra: Rand<A>,
    rb: Rand<B>,
    f: (A, B) -> C,
): Rand<C> = { rnd ->
    val (va, first) = ra.invoke(rnd)
    val (vb, second) = rb.invoke(first)
    f(va, vb) to second
}

fun <A, B> flatMap(f: Rand<A>, g: (A) -> Rand<B>): Rand<B> = { rnd ->
    val (va, stateA) = f(rnd)
    g(va)(stateA)
}


fun nonNegativeLessThan(n: Int): Rand<Int> =
    flatMap(::nonNegativeInt) { value ->
        val mod = value % n
        if (value + (n - 1) - mod >= 0) unit(mod)
        else nonNegativeLessThan(n)
    }


fun <A, B> mapReimplement(x: Rand<A>, f: (A) -> B): Rand<B> = flatMap(x) { value -> unit(f(value)) }
fun <A, B, C> map2Reimplement(x: Rand<A>, y: Rand<B>, f: (A, B) -> C): Rand<C> =
    flatMap(x) { aVal -> mapR(y) { bVal -> f(aVal, bVal) } }


fun <A> sequence(fs: List<Rand<A>>): Rand<List<A>> = { rng ->
    val (rng1, values) = fs.fold(Pair(rng, emptyList<A>())) { acc, elem ->
        val (currentState, currentValues) = acc
        val (value, newState) = elem.invoke(currentState)
        newState to (currentValues + value)
    }
    values to rng1
}

fun <A> sequence2(fs: List<Rand<A>>): Rand<List<A>> =
    fs.foldRight(unit(emptyList<A>())) { elem, acc ->
        map2(elem, acc) { h, t -> listOf(h) + t }
    }


fun <A> sequence3(fs: List<Rand<A>>): Rand<List<A>> = { rng ->
    when {
        fs.isEmpty() -> unit(emptyList<A>())(rng)
        else -> {
            val (value, newState) = fs.first().invoke(rng)
            val (finalValue, finalState) = sequence3(fs.subList(1, fs.size))(newState)
            (listOf(value) + finalValue) to finalState
        }
    }
}


fun doubleInt(rng: RNG): Pair<Pair<Double, Int>, RNG> {
    val (pair, rng1) = intDouble(rng)
    val (i, d) = pair
    return Pair(Pair(d, i), rng1)

}


fun double3(rng: RNG): Pair<Triple<Double, Double, Double>, RNG> {

    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    return Pair(Triple(d1, d2, d3), rng3)
}

fun ints(count: Int, rng: RNG): Pair<List<Int>, RNG> {
    fun go(index: Int, rng: RNG, acc: List<Int>): Pair<List<Int>, RNG> =
        if (index == count) {
            Pair(acc, rng)
        } else {
            val (i, rng1) = rng.nextInt()
            go(index + 1, rng1, acc + i)
        }


//    return if (count > 0) {
//        val (i, rng1) = rng.nextInt()
//        val (ints, rng2) = ints(count - 1, rng1)
//        Pair(listOf(i) + ints, rng2)
//    } else {
//        Pair(emptyList<Int>(), rng)
//    }
    return go(0, rng, emptyList())
}


fun ints2(count: Int, rng: RNG): Pair<List<Int>, RNG> {
    val map: List<Rand<Int>> = (1..count).map { intR }
    return sequence(map)(rng)
}

fun ints3(count: Int): Rand<List<Int>> {
    val map: List<Rand<Int>> = (1..count).map { intR }
    return sequence(map)
}

data class State<S, out A>(val run: (S) -> Pair<A, S>) : StateOf<S, A> {
    fun <B> flatMap(f: (A) -> State<S, B>): State<S, B> = State { s: S ->
        val (value, state) = this.run(s)
        f(value).run(state)
    }

    fun <B> map(f: (A) -> B): State<S, B> = flatMap { a -> unit<S, B>(f(a)) }

    fun <S> get(): State<S, S> =
        State { s -> s to s }

    fun setState(s: S): State<S, Unit> = State { Unit to s }

    companion object {
        fun <S, A> unit(a: A) = State { s: S -> Pair(a, s) }

        fun <S, A, B, C> map2(a: State<S, A>, b: State<S, B>, f: (A, B) -> C): State<S, C> =
            a.flatMap { valueA -> b.map { valueB -> f(valueA, valueB) } }

        fun <S, A> sequence(l: List<State<S, A>>): State<S, List<A>> = l.foldRight(unit(emptyList())) { elem, acc ->
            map2(elem, acc) { value, list -> listOf(value) + list }
        }

        fun <S> get(): State<S, S> =
            State { s -> s to s }

        fun <S> set(s: S): State<S, Unit> = State { Unit to s }
    }

}

class RngTest {
    @Test
    fun `testNextInt`() {
        val simpleRNG = SimpleRNG(3)
        assertThat(simpleRNG.nextInt(), Matchers.`is`(simpleRNG.nextInt()))
    }

    @Test
    fun `testNextInt2`() {
        val simpleRNG = SimpleRNG(3)
        assertThat(simpleRNG.nextInt().second.nextInt(), Matchers.not(simpleRNG.nextInt()))
    }

    @Test
    fun `testInts`() {
        val simpleRNG = SimpleRNG(3)

        val (ints, rng) = ints2(3, simpleRNG)
        assertThat(ints.toSet(), Matchers.hasSize(3))
        assertThat(rng, Matchers.`is`(SimpleRNG(150025546934108L)))

    }

    @Test
    fun `testMap`() {
        val simpleRNG = SimpleRNG(3)

        val (i1, newState) = simpleRNG.nextInt()

        val map = mapR({ rng -> rng.nextInt() }, { it.toString() })
        val (v, rng1) = map(simpleRNG)


        assertThat(v, Matchers.`is`(i1.toString()))
        assertThat(rng1, Matchers.`is`(newState))
    }


    @Test
    fun `testMap2`() {
        val simpleRNG = SimpleRNG(3)

        val map2 = map2(intR, intR) { a, b -> a to b }
        val (pair, rng) = map2(simpleRNG)

        assertThat(pair, Matchers.`is`(Pair(1154246, 832745806)))
        assertThat(rng, Matchers.`is`(SimpleRNG(54574829143541L)))
    }

    @Test
    fun `testSequence`() {
        val simpleRNG = SimpleRNG(3)
        val sequence = sequence(listOf(nonNegativeOdd(), nonNegativeEven()))
        val (actualInts, actualState) = sequence(simpleRNG)

        assertThat(actualInts, Matchers.`is`(listOf(1154247, 832745806)))
        assertThat(actualState, Matchers.`is`(SimpleRNG(54574829143541L)))
    }


}