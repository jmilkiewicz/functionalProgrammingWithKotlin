import Option.Companion.orElse
import arrow.core.ForListK
import arrow.core.ForOption
import arrow.core.ListKOf
import arrow.core.OptionOf
import arrow.core.compose
import arrow.core.extensions.either.applicative.applicative
import arrow.core.fix
import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers.hasEntry
import org.hamcrest.Matchers.`is`
import org.junit.jupiter.api.Test
import java.util.concurrent.Executors
import kotlin.math.min

interface Monoid<A> {
    fun combine(a1: A, a2: A): A
    val nil: A
}


val stringMonoid = object : Monoid<String> {

    override fun combine(a1: String, a2: String): String = a1 + a2

    override val nil: String = ""
}

fun <A> listMonoid(): Monoid<List<A>> = object : Monoid<List<A>> {
    override fun combine(a1: List<A>, a2: List<A>): List<A> = a1 + a2

    override val nil: List<A> = emptyList()
}

fun intAddition(): Monoid<Int> =
    object : Monoid<Int> {
        override fun combine(a1: Int, a2: Int): Int = a1 + a2
        override val nil: Int = 0
    }


fun intMultiplication(): Monoid<Int> =
    object : Monoid<Int> {
        override fun combine(a1: Int, a2: Int): Int = a1 * a2
        override val nil: Int = 1
    }


fun booleanOr(): Monoid<Boolean> =
    object : Monoid<Boolean> {
        override fun combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
        override val nil: Boolean = false
    }


fun booleanAnd(): Monoid<Boolean> = object : Monoid<Boolean> {
    override fun combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override val nil: Boolean = true
}

fun <A> optionMonoid(): Monoid<Option<A>> = object : Monoid<Option<A>> {
    override fun combine(a1: Option<A>, a2: Option<A>): Option<A> = a1.orElse { a2 }

    override val nil: Option<A> = None
}


fun <A> dual(m: Monoid<A>): Monoid<A> = object : Monoid<A> {
    override fun combine(a1: A, a2: A): A {
        return m.combine(a2, a1)
    }

    override val nil: A
        get() = m.nil
}


fun <A> endoMonoid(): Monoid<(A) -> A> = object : Monoid<(A) -> A> {
    override fun combine(a1: (A) -> A, a2: (A) -> A): (A) -> A {
        return a1 compose a2
    }

    override val nil: (A) -> A = { a -> a }
}

fun <A> concatenate(la: List<A>, m: Monoid<A>): A = la.fold(m.nil) { acc, a -> m.combine(acc, a) }


fun <A, B> foldMap(la: List<A>, m: Monoid<B>, f: (A) -> B): B = la.fold(m.nil) { acc, e -> m.combine(acc, f(e)) }


fun <A, B> foldRight(la: List<A>, z: B, f: (A, B) -> B): B {
    return foldMap(la, endoMonoid()) { a: A -> { b: B -> f(a, b) } }(z)
}

fun <A, B> foldMapR(la: List<A>, m: Monoid<B>, f: (A) -> B): B {
    if (la.size == 1) {
        return f(la.first())
    }
    if (la.isEmpty()) {
        return m.nil
    }
    val (left, right) = la.splitAt(la.size / 2)
    return m.combine(foldMapR(left, m, f), foldMapR(right, m, f))
}


fun <A> par(m: Monoid<A>): Monoid<Par<A>> = object : Monoid<Par<A>> {
    override fun combine(par1: Par<A>, par2: Par<A>): Par<A> {
        return Pars2.map22(par1, par2) { a1, a2 -> m.combine(a1, a2) }
    }

    override val nil: Par<A> = Pars2.unit(m.nil)
}

fun <A, B> parFoldMap(
    la: List<A>,
    pm: Monoid<Par<B>>,
    f: (A) -> B,
): Par<B> {
    if (la.size == 1) {
        return Pars2.unit(f(la.first()))
    }
    if (la.isEmpty()) {
        return pm.nil
    }
    val (left, right) = la.splitAt(la.size / 2)
    return pm.combine(parFoldMap(left, pm, f), parFoldMap(right, pm, f))

}


//Niestety ten monoid nie spełnia praw :(
//988050756 1960703815 1062510111
val monoidForLess = object : Monoid<Pair<Int, Boolean>> {
    override fun combine(a1: Pair<Int, Boolean>, a2: Pair<Int, Boolean>): Pair<Int, Boolean> {
        val min = min(a2.first, a1.first)
        if (!a1.second) {
            return min to a1.second
        }
        if (!a2.second) {
            return min to a2.second
        }
        val lessThanNext = a1.first <= a2.first
        return min to lessThanNext
    }

    override val nil: Pair<Int, Boolean> = Pair(Int.MIN_VALUE, true)

}


fun ordered(ints: List<Int>): Boolean {

    val (i1, b) = foldMapR(ints, monoidForLess) { i -> i to true }
    println(i1)
    return b
}


sealed class WC

data class Stub(val chars: String) : WC()
data class Part(val ls: String, val words: Int, val rs: String) : WC()


fun wcMonoid(): Monoid<WC> = object : Monoid<WC> {
    override fun combine(a1: WC, a2: WC): WC {
        return when {
            a1 is Part && a2 is Part -> {
                var words = a1.words + a2.words +
                        if (a1.rs.isNotEmpty() || a2.ls.isNotEmpty()) {
                            1
                        } else
                            0
                Part(a1.ls, words, a2.rs)
            }

            a1 is Part && a2 is Stub -> a1.copy(rs = a1.rs + a2.chars)
            a1 is Stub && a2 is Part -> a2.copy(ls = a1.chars + a2.ls)
            a1 is Stub && a2 is Stub -> Stub(a1.chars + a2.chars)
            else -> throw Exception("wrong!!!")
        }

    }

    override val nil: WC = Stub("")

}


fun wordCount(s: String): Int {
    val foldMapR: WC = foldMapR(s.asSequence().toList(), wcMonoid()) {
        if (it == ' ') Part("", 0, "")
        else Stub(it.toString())
    }

    val isNotEmpty: (String) -> Int = { s -> if (s.isEmpty()) 0 else 1 }

    return when (foldMapR) {
        is Stub -> isNotEmpty(foldMapR.chars)
        is Part -> foldMapR.words + isNotEmpty(foldMapR.ls) + isNotEmpty(foldMapR.ls)
    }
}

interface Foldable<F> {

    fun <A, B> foldRight(fa: arrow.Kind<F, A>, z: B, f: (A, B) -> B): B =
        foldMap(fa, endoMonoid()) { a: A -> { b: B -> f(a, b) } }(z)

    fun <A, B> foldLeft(fa: arrow.Kind<F, A>, z: B, f: (B, A) -> B): B =
        foldMap(fa, dual(endoMonoid())) { a: A -> { b: B -> f(b, a) } }(z)

    fun <A, B> foldMap(fa: arrow.Kind<F, A>, m: Monoid<B>, f: (A) -> B): B =
        foldRight(fa, m.nil) { a, b -> m.combine(f(a), b) }

    fun <A> concatenate(fa: arrow.Kind<F, A>, m: Monoid<A>): A =
        foldLeft(fa, m.nil, m::combine)

    fun <A> toList(fa: arrow.Kind<F, A>): List<A> = foldLeft(fa, emptyList()) { acc, elem -> acc + listOf(elem) }
}

object ListFoldable : Foldable<ForListK> {

    override fun <A, B> foldRight(
        fa: ListKOf<A>,
        z: B,
        f: (A, B) -> B,
    ): B =
        fa.fix().foldRight(z, f)

    override fun <A, B> foldLeft(
        fa: ListKOf<A>,
        z: B,
        f: (B, A) -> B,
    ): B =
        fa.fix().foldLeft(z, f)

    fun <A> bag(la: List<A>): Map<A, Int> {

        val mapMergeMonoid: Monoid<Map<A, Int>> = mapMergeMonoid(intAddition())

        return foldMapR(la, mapMergeMonoid) { la -> mapOf(la to 1) }
    }

}

object OptionFoldable : Foldable<ForOption> {

    override fun <A, B> foldMap(
        fa: OptionOf<A>,
        m: Monoid<B>,
        f: (A) -> B,
    ): B =
        when (val o = fa.fix()) {
            is arrow.core.None -> m.nil
            is arrow.core.Some -> f(o.value)
        }
}


fun <A, B> productMonoid(
    ma: Monoid<A>,
    mb: Monoid<B>,
): Monoid<Pair<A, B>> = object : Monoid<Pair<A, B>> {
    override fun combine(a1: Pair<A, B>, a2: Pair<A, B>): Pair<A, B> {
        val combinedA = ma.combine(a1.first, a2.first)
        val combinedB = mb.combine(a1.second, a2.second)
        return combinedA to combinedB
    }

    override val nil: Pair<A, B>
        get() = ma.nil to mb.nil

}


fun <K, V> mapMergeMonoid(v: Monoid<V>): Monoid<Map<K, V>> =
    object : Monoid<Map<K, V>> {
        override fun combine(a1: Map<K, V>, a2: Map<K, V>): Map<K, V> =
            (a1.keys + a2.keys).fold(nil) { acc, k ->
                acc + mapOf(
                    k to v.combine(
                        a1.getOrDefault(k, v.nil),
                        a2.getOrDefault(k, v.nil)
                    )
                )
            }

        override val nil: Map<K, V> = emptyMap()
    }


fun <A, B> functionMonoid(b: Monoid<B>): Monoid<(A) -> B> = object : Monoid<(A) -> B> {
    override fun combine(a1: (A) -> B, a2: (A) -> B): (A) -> B = { a: A ->
        b.combine(a1(a), a2(a))
    }

    override val nil: (A) -> B
        get() = { b.nil }
}


class TestMonoid {
    @Test
    fun `testParFoldMap`() {
        val es = Executors.newFixedThreadPool(10)

        val value = parFoldMap(
            listOf("lorem", "ipsum", "dolor", "sit"),
            par(stringMonoid)
        ) {
            it.uppercase()
        }(es).get()

        assertThat(value, `is`("LOREMIPSUMDOLORSIT"))
    }


    @Test
    fun `testOrdered`() {

        val ordered = ordered(listOf(988050756, 1960703815, 1062510111))
        assertThat(ordered, `is`(false))
    }

    fun <A> monoidLaws(m: Monoid<A>, gen: GenN<A>) =
        forAllN(
            gen.flatMap { a ->
                gen.flatMap { b ->
                    gen.map { c ->
                        Triple(a, b, c)
                    }
                }
            }
        ) { (a, b, c) ->
            val x = m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)
            val y = m.combine(m.nil, a) == m.combine(a, m.nil)
            val z = m.combine(m.nil, a) == a

            x && y && z
        }


    @Test
    fun `testMonoidForLess`() {
        val gen = genNonNegativeInt.map { i -> i to true }

        runN(monoidLaws(monoidForLess, gen))
    }

    @Test
    fun `testWCMonoid`() {


        val stringLengthGen = chooseN(0, 3)

        val genParts: GenN<WC> = genString(stringLengthGen).flatMap { string1 ->
            genString(stringLengthGen).flatMap { string2 ->
                chooseN(1, 5).map { i -> Part(string1, i, string2) }
            }
        }


        val genStubs: GenN<WC> = genString(stringLengthGen).map { Stub(it) }

        val eitherStubOrPart = GenN.union(genParts, genStubs)


        runN(monoidLaws(wcMonoid(), eitherStubOrPart))
    }

    @Test
    fun `testCountWords`() {

//        assertThat(wordCount("lorem ipsum dolor sit amet,"), Matchers.`is`(5))
//
//        assertThat(wordCount("     "), Matchers.`is`(0))
//        assertThat(wordCount("avc"), Matchers.`is`(1))

        assertThat(wordCount("a b c"), `is`(3))

    }

    @Test
    fun `testProductMonoid`() {

        val stringLengthGen = genString(chooseN(0, 10))
        val intGen = genNonNegativeInt


        val generatorOfPairs = stringLengthGen.combineN(intGen)

        runN(monoidLaws(productMonoid(stringMonoid, intAddition()), generatorOfPairs))
    }

    @Test
    fun `testMapMergeMonoidLaw`() {

        val stringLengthGen = GenN.unit("dupa")
        val listOfintGen = GenN.listOfN(chooseN(3, 5), genNonNegativeInt)


        val genOfMaps: GenN<Map<String, List<Int>>> = stringLengthGen.flatMap { key ->
            listOfintGen.map { values -> mapOf(key to values) }
        }

        runN(monoidLaws(mapMergeMonoid(listMonoid()), genOfMaps))
    }


    @Test
    fun `testMapMergeMonoid`() {
        val input = listOf(
            mapOf("huj" to listOf(1, 2), "wuj" to listOf(2, 3)),
            mapOf("huj" to listOf(3, 4), "zbój" to listOf(6)),
            mapOf("huj" to listOf(5), "zbój" to listOf(6)),
            mapOf("luj" to listOf(11))
        )


        val mapMergeMonoid = mapMergeMonoid<String, List<Int>>(listMonoid())

        val result = foldMapR(input, mapMergeMonoid) { it }

        assertThat(result, hasEntry(`is`("huj"), `is`(listOf(1, 2, 3, 4, 5))))
        assertThat(result, hasEntry(`is`("wuj"), `is`(listOf(2, 3))))
        assertThat(result, hasEntry(`is`("zbój"), `is`(listOf(6, 6))))
        assertThat(result, hasEntry(`is`("luj"), `is`(listOf(11))))
    }

    @Test
    fun `testBag`() {
        val result = ListFoldable.bag(listOf("a", "rose", "is", "a", "rose"))

        assertThat(result, hasEntry(`is`("a"), `is`(2)))
        assertThat(result, hasEntry(`is`("rose"), `is`(2)))
        assertThat(result, hasEntry(`is`("is"), `is`(1)))

    }


    @Test
    fun `testMeanOfList`() {

        val productMonoid = productMonoid(intAddition(), intAddition())

        val (sum, size) = foldMapR(listOf(1, 2, 3, 4), productMonoid) { e -> e to 1 }

        assertThat(sum, `is`(10))
        assertThat(size, `is`(4))
    }


}