import Gen.Companion.weighted
import Prop.Companion.forAll
import Prop.Companion.forAll2
import arrow.core.getOrElse
import arrow.core.toOption
import org.junit.jupiter.api.Test
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import kotlin.math.absoluteValue
import kotlin.math.max
import kotlin.math.min

fun <A> listOf(a: Gen<A>): List<Gen<A>> = TODO()


data class Gen<A>(val sample: State<RNG, A>) {
    fun unsized(): SGen<A> = SGen { this }

    companion object {
        fun <A> unit(a: A): Gen<A> = Gen(State.unit(a))
        fun <A> listOfN(gn: Gen<Int>, ga: Gen<A>): Gen<List<A>> = gn.flatMap { listOfN(it, ga) }
        fun <A> union(ga: Gen<A>, gb: Gen<A>): Gen<A> = boolean().flatMap { if (it) ga else gb }
        fun <A> weighted(
            pga: Pair<Gen<A>, Double>, pgb: Pair<Gen<A>, Double>
        ): Gen<A> {
            val (ga, p1) = pga
            val (gb, p2) = pgb
            val prob = p1.absoluteValue / (p1.absoluteValue + p2.absoluteValue)

            return Gen(State { rng: RNG -> double(rng) }).flatMap { d ->
                if (d < prob) ga else gb
            }
        }
    }

    fun <B> flatMap(f: (A) -> Gen<B>): Gen<B> = Gen(this.sample.flatMap { f.invoke(it).sample })

    fun listOf(): SGen<List<A>> = SGen { i -> listOfN(i, this) }

    fun <B> map(f: (A) -> B): Gen<B> = Gen(this.sample.map(f))

    infix fun <B> combine(gb: Gen<B>): Gen<Pair<A, B>> =
        map2G(this, gb) { s, a -> s to a }

}


data class SGen<A>(val forSize: (Int) -> Gen<A>) {

    operator fun invoke(i: Int): Gen<A> = this.forSize(i)


    fun <B> map(f: (A) -> B): SGen<B> = SGen { i -> mapG(this.forSize(i), f) }


    fun <B> flatMap(f: (A) -> Gen<B>): SGen<B> = SGen { i -> this.forSize(i).flatMap(f) }

    companion object {
        fun <A> listOf(a: Gen<A>): SGen<List<A>> = SGen { i -> listOfN(i, a) }
    }
}

typealias SuccessCount = Int
typealias FailedCase = String


fun choose(start: Int, stopExclusive: Int): Gen<Int> =
    Gen(State { rng: RNG -> double(rng) }.map { start + (it * (stopExclusive - start)) }.map { it.toInt() })


fun boolean(): Gen<Boolean> = Gen(State { rng -> nextBoolean(rng) })

fun genInt(): Gen<Int> = Gen(State { intR(it) })

fun <A> listOfN(n: Int, ga: Gen<A>): Gen<List<A>> {
    val map: List<State<RNG, A>> = (1..n).map { ga.sample }
    val sequence: State<RNG, List<A>> = State.sequence(map)
    return Gen(sequence)
}

fun <A, B> mapG(ga: Gen<A>, f: (A) -> B): Gen<B> = Gen(ga.sample.map(f))

fun <A, B, C> map2G(ga: Gen<A>, gb: Gen<B>, f: (A, B) -> C): Gen<C> = Gen(State.map2(ga.sample, gb.sample, f))


fun string(x: Int): Gen<List<Char>> = listOfN(x, mapG(choose(33, 126)) { it.toChar() })


sealed class ResultG {
    abstract fun isFalsified(): Boolean
}

object Passed : ResultG() {
    override fun isFalsified(): Boolean = false
}

data class Falsified(
    val failure: FailedCase, val successes: SuccessCount
) : ResultG() {
    override fun isFalsified(): Boolean = true
}

object Proved : ResultG() {
    override fun isFalsified(): Boolean = false
}

//interface Prop {
//    fun check(): Either<Pair<FailedCase, SuccessCount>, SuccessCount>
//    fun and(p: Prop): Prop {
//        val bothChecks = this.check().flatMap { _ -> p.check() }
//        return object : Prop {
//            override fun check(): Either<Pair<FailedCase, SuccessCount>, SuccessCount> {
//                return bothChecks
//            }
//
//        }
//    }
//}

data class Prop(val check: (MaxSize, TestCases, RNG) -> ResultG) {
    fun and(p: Prop): Prop = Prop { max, n, rng ->
        when (val prop = check(max, n, rng)) {
            is Falsified -> prop
            else -> p.check(max, n, rng)
        }
    }

//    fun or(other: Prop) = Prop { n, rng ->
//        when (val prop = run(n, rng)) {
//            is Falsified -> other.tag(prop.failure).run(n, rng)
//
//            is Passed -> prop
//        }
//    }
//
//    private fun tag(msg: String) = Prop { n, rng ->
//        when (val prop = run(n, rng)) {
//            is Falsified -> Falsified(
//                "$msg: ${prop.failure}", prop.successes
//            )
//
//            is Passed -> prop
//        }
//    }

    companion object {
        fun check(p: () -> Boolean): Prop = Prop { _, _, _ ->
            if (p()) Proved else Falsified("dupa", 0)
        }


        fun <A> forAll(ga: Gen<A>, f: (A) -> Boolean): Prop = Prop { _: MaxSize, n: TestCases, rng: RNG ->
            randomSequence(ga, rng).mapIndexed { i, a ->
                try {
                    if (f(a)) Passed
                    else Falsified(a.toString(), i)
                } catch (e: Exception) {
                    Falsified(buildMessage(a, e), i)
                }
            }.take(n).find { it.isFalsified() }.toOption().getOrElse { Passed }
        }

        private fun <A> randomSequence(
            ga: Gen<A>, rng: RNG
        ): Sequence<A> = sequence {
            val (a: A, rng2: RNG) = ga.sample.run(rng)
            yield(a)
            yieldAll(randomSequence(ga, rng2))
        }

        private fun <A> buildMessage(a: A, e: Exception) = """
                |test case: $a
                |generated and exception: ${e.message}
                |stacktrace:
                |${e.stackTrace.joinToString("\n")}
            """.trimMargin()

        fun <A> forAll2(a: Gen<A>, f: (A) -> Boolean): Prop = Prop { testcase, _, rng ->

            fun x(i: Int, rng: RNG): ResultG {
                if (i == testcase) {
                    return Passed
                }
                val (value, newRng) = a.sample.run(rng)
                if (f(value)) {
                    return x(i + 1, newRng)
                } else {
                    return Falsified(value.toString(), i)
                }
            }

            val x = x(0, rng)
            x
        }


        fun <A> forAll2(g: SGen<A>, f: (A) -> Boolean): Prop = forAll({ i -> g(i) }, f)

        fun <A> forAll(g: (Int) -> Gen<A>, f: (A) -> Boolean): Prop = Prop { max, n, rng ->

            val casePerSize: Int = (n + (max - 1)) / max

            val props: Sequence<Prop> =
                generateSequence(0) { it + 1 }.take(min(n, max) + 1).map { i -> forAll(g(i), f) }

            val prop: Prop = props.map { p ->
                Prop { max, _, rng ->
                    p.check(max, casePerSize, rng)
                }
            }.reduce { p1, p2 -> p1.and(p2) }

            prop.check(max, n, rng)
        }
    }

}

typealias MaxSize = Int
typealias TestCases = Int

class SomeTest {


    val smallInt = choose(-10, 10)


    fun <A> nonEmptyListOf(ga: Gen<A>): SGen<List<A>> = SGen { i -> listOfN(max(1, i), ga) }


    fun maxProp(): Prop = Prop.forAll2(nonEmptyListOf(smallInt)) { ns ->
        val mx = ns.max()
        ns.none { it > mx }
    }

    fun sorted(): Prop = Prop.forAll2(nonEmptyListOf(smallInt)) { ns ->
        val sorted = ns.sorted()
        val lessOrEqualToSuccessor = sorted.zipWithNext().fold(true) { acc, pair ->
            acc && pair.first <= pair.second
        }
        lessOrEqualToSuccessor && ns.containsAll(sorted) && sorted.containsAll(ns)

    }


    fun unitPars(ga: Gen<Int>): Gen<Pair<Int, Par<Int>>> = mapG(ga) { a -> a to Pars.unit(a) }

    val es = Executors.newCachedThreadPool()
    val p1 = forAll(Gen.unit(Pars.unit(1))) { pi ->
        map(pi, { it + 1 })(es).get() == Pars.unit(2)(es).get()
    }


    val pMoje = forAll(unitPars(smallInt)) { (i, par) ->
        map(par) { it + 1 }(es).get() == Pars.unit(i + 1)(es).get()
    }

    fun run(
        p: Prop, maxSize: Int = 5, testCases: Int = 5, rng: RNG = SimpleRNG(3)
    ): Unit = when (val result = p.check(maxSize, testCases, rng)) {
        is Falsified -> println(
            "Falsified after ${result.successes}" + "passed tests: ${result.failure}"
        )

        is Passed -> println("OK, passed $testCases tests.")

        is Proved -> println("OK, proved property.")
    }

    fun <A> equal(p1: Par<A>, p2: Par<A>): Par<Boolean> = Pars.map2(p1, p2) { a, b -> a == b }


    val ges: Gen<ExecutorService> = weighted(choose(1, 4).map {
        Executors.newFixedThreadPool(it)
    } to .75, Gen.unit(
        Executors.newCachedThreadPool()
    ) to .25)

    fun <A> forAllPar(ga: Gen<A>, f: (A) -> Par<Boolean>): Prop =
        forAll(ges combine ga) { (es, a) ->
            f(a)(es).get()
        }


    fun checkPar(p: Par<Boolean>) = forAllPar(Gen.unit(Unit)) { p }

    val p2 = checkPar(
        equal(
            map(Pars.unit(1)) { it + 1 },
            Pars.unit(2)
        )
    )

    fun genIntBooleanFn(t: Int): Gen<(Int) -> Boolean> =
        Gen.unit { i: Int -> i > t }


    @Test
    fun `testStringGeneration`() {
        val (chars, rng) = string(8).sample.run(SimpleRNG(100))
        println(chars.joinToString(separator = ""))
    }

    @Test
    fun `testMaxProperty`() {
        run(maxProp())
    }

    @Test
    fun `testSortedProperty`() {
        run(sorted())
    }


    @Test
    fun `testGoodNotesApproach`() {


        fun go(g: Gen<String>): Gen<String> {
            return g.flatMap { str ->

                if (str.length == 4) {
                    Gen.unit(str)
                } else {
                    val gen = Gen(State { s ->
                        val (value, nextState) = choose(0, 10).sample.run(s)
                        Pair(str + value.toString(), nextState)
                    })
                    go(gen)
                }
            }
        }

        val (value, rng) = go(Gen.unit("a")).sample.run(SimpleRNG(3))
        println(value)


//        val attempt = Gen.unit("a").flatMap { str ->
//            Gen(sample = State { s ->
//                val (_, rng) = s.nextInt()
//                Pair(str + "x", rng)
//            })
//        }
//            .flatMap { str -> Gen(sample = State { s -> Pair(str + "x", s) }) }
        //attempt.sample.run(SimpleRNG(100))
    }

    @Test
    fun `testcheck`() {

        run(Prop.check {
            val p1 = map(Pars.unit(1)) { it + 1 }
            val p2 = Pars.unit(2)
            equal(p1, p2).invoke(es).get()
        })
    }

    @Test
    fun `testMoje`() {
        run(pMoje)
    }

    @Test
    fun `testidentity`() {

        val pint: Gen<Par<Int>> =
            choose(0, 10).map {
                Pars.unit(it)
            }
        val identity = forAllPar(pint) { gen -> equal(map(gen) { it }, gen) }

        run(identity)
    }


    @Test
    fun `testidentityRich`() {

        val pint: Gen<Par<Int>> =
            choose(0, 10).flatMap { n -> listOfN(n, choose(-100, 100)) }
                .map { ls ->
                    ls.fold(Pars.unit(0)) { acc, i ->
                        Pars.map2(acc, Pars.unit(i)) { a, b -> a + b }
                    }
                }
        val identity = forAllPar(pint) { gen -> equal(map(gen) { it }, gen) }

        run(identity)
    }

    @Test
    fun `testFork`() {

        val pint: Gen<Par<Int>> =
            choose(0, 10).flatMap { n -> listOfN(n, choose(-100, 100)) }
                .map { ls ->
                    ls.fold(Pars.unit(0)) { acc, i ->
                        Pars.map2(acc, Pars.unit(i)) { a, b -> a + b }
                    }
                }
        val forkProp = forAllPar(pint) { gen -> equal(Pars.fork { gen }, gen) }

        run(forkProp)
    }

    @Test
    fun `testTakeWhilePoorMan`() {
        val isEven = { i: Int -> i % 2 == 0 }
        val sgen = SGen { i -> listOfN(i, choose(0, 10)) }

        val takeWhileProp = forAll2(sgen) { list ->
            val takeWhile = list.takeWhile(isEven)
            val firstNotMatching = list.subList(takeWhile.size, list.size).firstOrNull()
            takeWhile.all(isEven) && firstNotMatching?.let { fi -> !isEven(fi) } ?: true
        }
        run(takeWhileProp)

    }

    @Test
    fun `testGenIntBoolean`() {
        val greaterThan10Generator = genIntBooleanFn(10)
//za każdym razem generuję tą samą funkcję czyli ta generacja jest do dupy
        val forAll = forAll(greaterThan10Generator) { f ->
            val takeWhile = listOf(1, 2, 5, 6, 7).takeWhile(f)
            takeWhile.all { f(it) }
        }


        run(forAll)

    }

    @Test
    fun `testGenIntBoolean2`() {
        val gen =
            listOfN(100, choose(1, 100)).flatMap { ls: List<Int> ->
                choose(1, ls.size / 2).flatMap { threshold: Int ->
                    genIntBooleanFn(threshold).map { fn: (Int) -> Boolean ->
                        ls.takeWhile(fn).all(fn)
                    }
                }
            }

        run(forAll2(gen) { it == it })
    }


//    @Test
//    fun `testForAll`() {
//        val run = Prop.forAll(genInt()) { it % 2 == 0 }.run(5, SimpleRNG(100))
//        assertThat(run, Matchers.`is`(Falsified(failure = "419891633", successes = 1)))
//    }
//
//    @Test
//    fun `testForAll2`() {
//        val run = Prop.forAll2(genInt()) { it % 2 == 0 }.run(5, SimpleRNG(100))
//        assertThat(run, Matchers.`is`(Falsified(failure = "419891633", successes = 1)))
//    }


}