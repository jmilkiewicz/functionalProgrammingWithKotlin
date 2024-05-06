import Option.Companion.catches
import Option.Companion.flatMap
import Option.Companion.orElse
import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers
import org.hamcrest.Matchers.`is`
import org.junit.jupiter.api.Test

sealed class Option<out A> {

    companion object {
        fun <A, B> Option<A>.map(f: (A) -> B): Option<B> =
            when (this) {
                is None -> None
                is Some -> Some(f(this.get))
            }

        fun <A, B, C> map2(a: Option<A>, b: Option<B>, f: (A, B) -> C): Option<C> =
            a.flatMap { aVal -> b.map { bVal -> f(aVal, bVal) } }

        fun <A> sequence(xs: List<Option<A>>): Option<List<A>> =

            xs.fold(Some(emptyList())) { acc, elem ->
                map2(acc, elem) { list, elem -> list + elem }
            }

        fun <A, B> traverse(
            xa: List<A>,
            f: (A) -> Option<B>
        ): Option<List<B>> =
            xa.fold(Some(emptyList())) { acc, elem ->
                val va = f(elem)
                when (va) {
                    is None -> None
                    is Some -> acc.map { l -> l + va.get }
                }

            }

        fun <A, B> traverse2(
            xa: List<A>,
            f: (A) -> Option<B>
        ): Option<List<B>> =
            when {
                xa.isEmpty() -> Some(emptyList())
                else -> map2(f(xa.first()), traverse2(xa.subList(1, xa.size), f)) { e, ac ->  listOf(e) +ac }
            }


        fun <A> sequence2(xs: List<Option<A>>): Option<List<A>> = traverse2(xs) {it}

        fun <A, B> Option<A>.flatMap(f: (A) -> Option<B>): Option<B> =
            this.map(f).getOrElse { None }


        fun <A> Option<A>.getOrElse(default: () -> A): A =
            when (this) {
                is None -> default()
                is Some -> this.get
            }


        fun <A> Option<A>.orElse(ob: () -> Option<A>): Option<A> = this.map { Some(it) }.getOrElse { ob() }


        fun <A> Option<A>.filter(f: (A) -> Boolean): Option<A> = this.flatMap {
            val f1 = f(it)
            if (f1) this else None
        }

        fun <A, B> lift(f: (A) -> B): (Option<A>) -> Option<B> = { oa -> oa.map(f) }

        fun <A> catches(f: () -> A): Option<A> =
            try {
                Some(f())
            } catch (e: Exception) {
                None
            }


    }

}


data class Some<out A>(val get: A) : Option<A>()

object None : Option<Nothing>()


class OptionTest {
    @Test
    fun `testflatMap`() {
        assertThat(Some(12).flatMap { Some(14) }, `is`(Some(14)))
    }

    @Test
    fun `testOrElse`() {
        assertThat(None.orElse { Some(14) }, `is`(Some(14)))
    }

    @Test
    fun `testLift`() {
        val divBy2 = { a: Int -> a / 2 }

        val lifted = Option.lift(divBy2)

        assertThat(lifted(Some(10)), Matchers.`is`(Some(5)))
    }

    @Test
    fun `testSequence`() {
        val input = listOf(Some(1), Some(3), Some(4))
        assertThat(Option.sequence(input), Matchers.`is`(Some(listOf(1, 3, 4))))
    }


    @Test
    fun `testSequence2`() {
        val input = listOf(Some(1), Some(3), Some(4), None)
        assertThat(Option.sequence(input), Matchers.`is`(None))
    }

    @Test
    fun `traverseTest`() {
        val input = listOf(1, 3, 4)
        assertThat(Option.traverse2(input) { Some(it) }, Matchers.`is`(Some(listOf(1, 3, 4))))
    }

    @Test
    fun `traverseTest2`() {
        val input = listOf(1, 3, 4, 0)
        assertThat(Option.traverse2(input) { catches { 10 / it } }, Matchers.`is`(None))
    }
}