import GenN.Companion.listOfN
import GenN.Companion.weighted
import arrow.core.getOrElse
import arrow.core.toOption
import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers
import org.hamcrest.Matchers.both
import org.hamcrest.Matchers.everyItem
import org.hamcrest.Matchers.greaterThanOrEqualTo
import org.hamcrest.Matchers.lessThan
import org.junit.jupiter.api.Test
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import kotlin.math.absoluteValue
import kotlin.math.max
import kotlin.math.min

typealias SuccessCountN = Int
typealias FailedCaseN = String


sealed class ResultN {
    abstract fun isFalsified(): Boolean
}

object PassedN : ResultN() {
    override fun isFalsified(): Boolean = false
}

data class FalsifiedN(
    val failure: FailedCase, val successes: SuccessCount,
) : ResultN() {
    override fun isFalsified(): Boolean = true
}

object ProvedN : ResultN() {
    override fun isFalsified(): Boolean = false
}


data class PropN(val check: (MaxSize, TestCases, RNG) -> ResultN) {
    fun and(p: PropN): PropN = PropN { maxSize, testCases, rng ->
        val check1 = this.check(maxSize, testCases, rng)
        if (!check1.isFalsified()) {
            p.check(maxSize, testCases, rng)
        } else {
            check1
        }
    }


    private fun tag(failure: FailedCase): PropN = PropN { max, testcases, rng ->
        when (val check = this.check(max, testcases, rng)) {
            is FalsifiedN -> check.copy(failure = "$failure: ${check.failure}")
            else -> PassedN
        }
    }

    fun or(p: PropN): PropN = PropN { maxSize, testcases, rng ->
        when (val check1 = this.check(maxSize, testcases, rng)) {
            is FalsifiedN -> p.tag(check1.failure).check(maxSize, testcases, rng)
            else -> check1
        }
    }


}


//jesli by Prop miało te parametry to check by musiało implementować logikę z forAllN
// czyli ten check byłby specyficzny dla forAll a nie dla innych "checkAll"
/*data class PropN2<A>(val ga: GenN<A>,val f: (A) -> Boolean){
    fun check(t: TestCases, rng: RNG) : ResultN = TODO()
}*/


fun <A> forAllN(ga: GenN<A>, f: (A) -> Boolean): PropN = PropN { maxSize: MaxSize, n: TestCases, rng: RNG ->
    randomSequence(ga, rng).mapIndexed { i, a ->
        try {
            if (f(a)) PassedN
            else FalsifiedN(a.toString(), i)
        } catch (e: Exception) {
            FalsifiedN(buildMessage(a, e), i)
        } catch (e: AssertionError) {
            FalsifiedN(buildMessage(a, e), i)
        }

    }.take(n).find { it.isFalsified() }.toOption().getOrElse { PassedN }
}


// moja wersja, nie jest lazy
fun <A> forAllNS(g: SGenN<A>, f: (A) -> Boolean): PropN = PropN { max: MaxSize, n: TestCases, rng: RNG ->

    val casePerSize: Int = (n + (max - 1)) / max
    val map: List<PropN> = (0..min(n, max) + 1).map { forAllN(g(it), f) }
    val fold: ResultN = map.fold(PassedN as ResultN) { acc, propN ->
        if (acc.isFalsified()) {
            acc
        } else {
            propN.check(max, casePerSize, rng)
        }
    }
    fold
}


fun <A> forAllNS2(g: SGenN<A>, f: (A) -> Boolean): PropN = PropN { max: MaxSize, n: TestCases, rng: RNG ->

    val casePerSize: Int = (n + (max - 1)) / max

    val props: Sequence<PropN> =
        generateSequence(0) { it + 1 }
            .take(min(n, max) + 1)
            .map { i -> forAllN(g(i), f) }

    val prop: PropN = props.map { p ->
        PropN { max, _, rng ->
            p.check(max, casePerSize, rng)
        }
    }.reduce { p1, p2 -> p1.and(p2) }

    prop.check(max, n, rng)
}

fun <A> checkAllN(ga: GenN<A>, f: (A) -> Unit): PropN = PropN { n: TestCases, maxSize: MaxSize, rng: RNG ->
    randomSequence(ga, rng).mapIndexed { i, a ->
        try {
            f(a)
            PassedN
        } catch (e: Exception) {
            FalsifiedN(buildMessage(a, e), i)
        } catch (e: AssertionError) {
            FalsifiedN(buildMessage(a, e), i)
        }
    }.take(n).find { it.isFalsified() }.toOption().getOrElse { PassedN }
}


private fun <A> randomSequence(
    ga: GenN<A>, rng: RNG,
): Sequence<A> = sequence {
    val (a: A, rng2: RNG) = ga.sample.run(rng)
    yield(a)
    yieldAll(randomSequence(ga, rng2))
}

private fun <A> buildMessage(a: A, e: Throwable) = """
    |test case: $a
    |generated and exception: ${e.message}
    |stacktrace:
    |${e.stackTrace.joinToString("\n")}
""".trimMargin()


data class GenN<A>(val sample: State<RNG, A>) {

    fun <B> map(f: (A) -> B): GenN<B> = GenN(this.sample.map(f))

    fun unsized(): SGenN<A> = SGenN { _ -> this }

    fun <B> flatMap(f: (A) -> GenN<B>): GenN<B> = GenN(this.sample.flatMap { a -> f(a).sample })


    fun listOf(): SGenN<List<A>> = SGenN { i -> listOfN(i, this) }

    companion object {


        fun <A> unit(a: A): GenN<A> = GenN(State.unit(a))

        fun boolean(): GenN<Boolean> = GenN(State(::nextBoolean))

        fun <A> listOfN(n: Int, ga: GenN<A>): GenN<List<A>> {
            val map: List<State<RNG, A>> = (0 until n).map { ga.sample }
            return GenN(State.sequence(map))
        }

        fun <A> listOfN(gn: GenN<Int>, ga: GenN<A>): GenN<List<A>> = gn.flatMap { size -> listOfN(size, ga) }

        fun <A> union(ga: GenN<A>, gb: GenN<A>): GenN<A> = boolean().flatMap { b -> if (b) ga else gb }

        fun <A, B, C> map2(ga: GenN<A>, gb: GenN<B>, f: (A, B) -> C): GenN<C> = GenN(
            State.map2(ga.sample, gb.sample) { a, b -> f(a, b) }
        )


        fun <A> weighted(
            pga: Pair<GenN<A>, Double>, pgb: Pair<GenN<A>, Double>,
        ): GenN<A> {
            val (ga, p1) = pga
            val (gb, p2) = pgb
            val prob = p1.absoluteValue / (p1.absoluteValue + p2.absoluteValue)

            return genDouble.flatMap { generated -> if (generated < prob) ga else gb }
        }

        fun double(intRange: IntRange) =
            GenN(State<RNG, Int> { rng -> nonNegativeInt(rng) }.map { it / (Int.MAX_VALUE.toDouble() + 1) })


        fun string(): GenN<String> = genString(7)



        fun choose(start: Int, stopExclusive: Int): GenN<Int> =
            GenN(State(nonNegativeLessThan(stopExclusive - start)).map { start + it })

    }
}


data class SGenN<A>(val forSize: (Int) -> GenN<A>) {

    operator fun invoke(i: Int): GenN<A> = this.forSize(i)


    fun <B> map(f: (A) -> B): SGenN<B> = SGenN { i -> this.forSize(i).map(f) }


    fun <B> flatMap(f: (A) -> GenN<B>): SGenN<B> = SGenN { i -> this.forSize(i).flatMap(f) }


}

fun chooseN(start: Int, stopExclusive: Int): GenN<Int> =
    GenN(State(nonNegativeLessThan(stopExclusive - start)).map { start + it })

fun pairOfInt(ga: GenN<Int>): GenN<Pair<Int, Int>> = GenN(State.map2(ga.sample, ga.sample) { i, j -> i to j })


infix fun <A, B> GenN<A>.combineN(gb: GenN<B>): GenN<Pair<A, B>> =
    GenN(State.map2(this.sample, gb.sample) { i, j -> i to j })


fun genString(length: Int): GenN<String> = GenN.listOfN(length, chooseN(97, 122))
    .map { listOfChars -> listOfChars.map { it.toChar() }.joinToString(separator = "") }

fun genString(length: GenN<Int>): GenN<String> = GenN.listOfN(length, chooseN(97, 122))
    .map { listOfChars -> listOfChars.map { it.toChar() }.joinToString(separator = "") }

fun genStrings(howMany: Int, stringSize: Int): GenN<List<String>> {
    return GenN.listOfN(howMany, genString(stringSize))
}


fun genIsGreaterThanX(x: Int): GenN<(Int) -> Boolean> =
    GenN.unit { i: Int -> i > x }

val genInt2 = GenN(State(intR))
val genNonNegativeInt = GenN(State { nonNegativeInt(it) })
val genDouble = GenN(State<RNG, Int> { rng -> nonNegativeInt(rng) }.map { it / (Int.MAX_VALUE.toDouble() + 1) })


fun runN(
    p: PropN,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = SimpleRNG(System.currentTimeMillis()),
): Unit =
    when (val result = p.check(maxSize, testCases, rng)) {
        is FalsifiedN ->
            println(
                "Falsified after ${result.successes}" +
                        "passed tests: ${result.failure}"
            )

        is PassedN ->
            println("OK, passed $testCases tests.")

        is ProvedN ->
            println("OK, proved")
    }


fun <A> nonEmptyListOf(ga: GenN<A>): SGenN<List<A>> = SGenN { a -> GenN.listOfN(max(1, a), ga) }


//jak dla unit testów
fun check(p: () -> Boolean): PropN = PropN { _, _, _ ->
    if (p()) ProvedN else FalsifiedN("failure :(", 0)
}

fun <A> equalN(p1: Par<A>, p2: Par<A>): Par<Boolean> = Pars2.map22(p1, p2) { a, b -> a == b }


val ges: GenN<ExecutorService> = weighted(
    chooseN(1, 4).map {
        Executors.newFixedThreadPool(it)
    } to .75,
    GenN.unit(
        Executors.newCachedThreadPool()
    ) to .25)


// Czy nie lepiej fun <A> forAllParN(ga: GenN<Par<A>>, f: (Par<A>) -> Par<Boolean>): PropN ?
fun <A> forAllParN(ga: GenN<A>, f: (A) -> Par<Boolean>): PropN {
    return forAllN(ga combineN ges) { (a, es) ->
        f(a).invoke(es).get()
    }
}

//Po co jest to checkPar skoro i tak nic nie generuje
fun checkPar(p: Par<Boolean>): PropN =
    forAllParN(GenN.unit(Unit)) { p }


class Gen2Test {

    @Test
    fun `testChooseN`() {
        val rng = SimpleRNG(2)

        val start = 3
        val stop = 5
        val (value, newRng) = chooseN(start, stop).sample.run(rng)
        println(value)
        assertThat(value, greaterThanOrEqualTo(start))
        assertThat(value, lessThan(stop))

    }

    @Test
    fun `testListOfN`() {
        val rng = SimpleRNG(2)

        val min = 1
        val max = 10000
        val size = 10
        val listGenStaticSize = GenN.listOfN(size, chooseN(min, max))

        val (fromStatic, endState1) = listGenStaticSize.sample.run(rng)

        assertThat(fromStatic.toSet().size, Matchers.`is`(size))
        assertThat(fromStatic, everyItem(both(greaterThanOrEqualTo(min)).and(lessThan(max))))


        val listGenDynamicSize = GenN.listOfN(GenN.unit(size), chooseN(min, max))

        val (fromDynamic, endState2) = listGenDynamicSize.sample.run(rng)

        assertThat(fromDynamic, Matchers.`is`(fromStatic))
        assertThat(endState2, Matchers.`is`(endState1))

    }

    @Test
    fun `testPairOfInt`() {
        val rng = SimpleRNG(2)


        val generator = pairOfInt(genInt2)
        val (pair, rng1) = generator.sample.run(rng)
        println(rng1)
        assertThat(pair.toList().toSet().size, Matchers.`is`(2))
    }

    @Test
    fun `testGenString`() {
        val rng = SimpleRNG(2)


        val length = 8
        val generator = genString(length)
        val (str, rng1) = generator.sample.run(rng)
        println(str)

        assertThat(str, Matchers.hasLength(length))
    }

    @Test
    fun `testGenIntInRow`() {
        val rng = SimpleRNG(2)


        val (i, rng1) = genInt2.sample.run(rng)
        val (j, rng2) = genInt2.sample.run(rng1)

        assertThat(setOf(i, j), Matchers.hasSize(2))
        assertThat(rng1, Matchers.not(rng2))
    }


    @Test
    fun `testForAllN`() {
        val rng = SimpleRNG(2)
        val min = 1
        val max = 10000

        val ints = chooseN(min, max)
        val check = forAllN(ints) { input ->
            input in min..<10000
        }.check(10000, 1, rng)

        assertThat(check, Matchers.`is`(PassedN))

    }

    @Test
    fun `testListOf`() {
        val rng = SimpleRNG(2)
        val size = 5

        val (ints, rng1) = chooseN(1, 10000).listOf().forSize(size).sample.run(rng)
        assertThat(ints, Matchers.hasSize(size))

    }

    @Test
    fun `testForMaxOfList`() {
        val smallInt = chooseN(-10, 10)
        val maxProp = forAllNS2(nonEmptyListOf(smallInt)) { ns ->
            val mx = ns.max()
            !ns.all { it > mx }
        }

        runN(maxProp)
    }

    @Test
    fun `testGenStrings`() {
        val rng = SimpleRNG(2)


        val maxProp = forAllNS(genString(5).listOf()) { ns ->
            println(ns)
            true
        }.check(3, 10, rng)
        // maxSize - do ilu może wzrosnąć lista - +1,
        // testCaseSize - do wzoru ile razy wygenerować listę o danym rozmiarze

    }

    @Test
    fun `testSorted`() {
        val sorted = forAllNS(nonEmptyListOf(genInt2)) { ns ->
            val sorted = ns.sorted()
            val areElementsSorted = (listOf(Int.MIN_VALUE) + sorted).zipWithNext().map { (a, b) -> a <= b }
                .fold(true) { c1, c2 -> c1 && c2 }
            areElementsSorted && ns.containsAll(sorted) && sorted.containsAll(ns)
        }
        runN(sorted)
    }

    @Test
    fun `testMapWithUnit`() {
        //we wanna check map(unit(1)) { it + 1 } == unit(2)
        val es = Executors.newCachedThreadPool()
        val result = forAllN(chooseN(0, 100)) { value ->
            Pars2.map(Pars2.unit(value)) { it + 1 }.invoke(es).get() == Pars2.unit(value + 1).invoke(es).get()
        }.check(-1, 10, SimpleRNG(3))

        assertThat(result, Matchers.`is`(PassedN))
    }

    @Test
    fun `testMapWithUnitLikeUnitTest`() {
        //we wanna check map(unit(1)) { it + 1 } == unit(2)
        val es = Executors.newCachedThreadPool()
        val result =
            check {
                val p1 = Pars2.map(Pars2.unit(1)) { it + 1 }
                val p2 = Pars2.unit(2)
                equalN(p1, p2).invoke(es).get()
            }.check(
                0,
                0,
                SimpleRNG(3)
            )

        assertThat(result, Matchers.`is`(ProvedN))
    }

    @Test
    fun testMapWithUnitViaCheckPar() {
        //using checkPar we check map(unit(1)) { it + 1 } == unit(2)

        runN(
            checkPar(
                equalN(
                    map(Pars2.unit(1)) { it + 1 },
                    Pars2.unit(2)
                )
            )
        )
    }

    @Test
    fun testMapWithUnitPrzechodinie() {
        //map(unit(x), f) == unit(f(x)) czyli map(y, id) == y

        val pint: GenN<Par<Int>> =
            chooseN(0, 10).map {
                Pars2.unit(it)
            }


        runN(forAllParN(pint) { par -> equalN(par, map(par) { it }) })
    }


    @Test
    fun testMapWithUnitFancy() {
        //map(unit(x), f) == unit(f(x)) czyli map(y, id) == y


        val gen: GenN<Par<List<Int>>> = chooseN(0, 4).flatMap { i -> listOfN(i, genNonNegativeInt) }
            .map { l -> Pars2.sequentialMap(l) { x -> x + 1 } }




        runN(forAllParN(gen) { par -> equalN(par, map(par) { it }) })
    }

    @Test
    fun generateFunctions() {

        //Przykład z książki - straszne gówno bo jak się wywali to nie wiadomo jakie były wygenerowane wartości
        val convolutedBoolean = listOfN(10, chooseN(1, 1000)).flatMap { l ->
            chooseN(1, l.size / 2).flatMap { threshold ->
                genIsGreaterThanX(threshold).map { f ->
                    val taken = l.takeWhile(f)
                    taken.all(f) && l.drop(taken.size).firstOrNull()?.let { f(it) } ?: true
                }

            }
        }

        runN(forAllN(convolutedBoolean) { it })


    }

    @Test
    fun generateFunctions2() {

        //Tu lepiej niż w teście wyżej ale jak się wywali to nie mam wartości parametrów funkcji i muszę patrzeć w kod (wartość 12)

        val myGen: GenN<Pair<List<Int>, (Int) -> Boolean>> =
            listOfN(10, chooseN(5, 1000)) combineN genIsGreaterThanX(12)

        runN(forAllN(myGen) { (list, predicate) ->
            val taken = list.takeWhile(predicate)
            taken.all(predicate) && list.drop(taken.size).firstOrNull()?.let { predicate(it).not() } ?: true
        })
    }


    @Test
    fun generateFunctionsCheckAll() {

        //to samo co wyżej ale używamy assercji - lepie widać błędy

        val myGen: GenN<Pair<List<Int>, (Int) -> Boolean>> = listOfN(10, chooseN(5, 100)) combineN genIsGreaterThanX(12)

        runN(checkAllN(myGen) { (list, predicate) ->
            val taken = list.takeWhile(predicate)

            assertThat(taken, everyItem(Matchers.greaterThan(12)))
            val firstNonMatchingOrNull = list.drop(taken.size).firstOrNull()
            if (firstNonMatchingOrNull != null) {
                assertThat(firstNonMatchingOrNull, Matchers.lessThanOrEqualTo(12))
            }

        })
    }

    @Test
    fun checkFork() {
        val pint: GenN<Par<Int>> =
            chooseN(0, 10).map {
                Pars2.unit(it)
            }


        forAllParN(pint) { x ->
            equalN(fork { x }, x)
        }
    }


}