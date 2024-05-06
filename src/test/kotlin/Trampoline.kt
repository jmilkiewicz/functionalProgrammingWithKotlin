import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers.`is`
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows

sealed class Tailrec<A> {
    fun <B> flatMap(f: (A) -> Tailrec<B>): Tailrec<B> = FlatMap(this, f)
    fun <B> map(f: (A) -> B): Tailrec<B> = flatMap { a -> Return(f(a)) }
}

data class Return<A>(val a: A) : Tailrec<A>()
data class Suspend<A>(val resume: () -> A) : Tailrec<A>()
data class FlatMap<A, B>(
    val sub: Tailrec<A>,
    val f: (A) -> Tailrec<B>,
) : Tailrec<B>()


@Suppress("UNCHECKED_CAST")
tailrec fun <A> run(tailRec: Tailrec<A>): A =
    when (tailRec) {
        is Return -> tailRec.a
        is Suspend -> tailRec.resume()
        is FlatMap<*, *> -> {
            val x = tailRec.sub as Tailrec<A>
            val f = tailRec.f as (A) -> Tailrec<A>
            when (x) {
                is Return ->
                    run(f(x.a))

                is Suspend ->
                    run(f(x.resume()))

                is FlatMap<*, *> -> {
                    val g = x.f as (A) -> Tailrec<A>
                    val y = x.sub as Tailrec<A>
                    run(y.flatMap { a: A -> g(a).flatMap(f) })
                }
            }
        }
    }

sealed class Async<A> {
    fun <B> flatMap(f: (A) -> Async<B>): Async<B> =
        FlatMapA(this, f)

    fun <B> map(f: (A) -> B): Async<B> =
        flatMap { a -> ReturnA(f(a)) }
}

data class ReturnA<A>(val a: A) : Async<A>()
data class SuspendA<A>(val resume: Par<A>) : Async<A>()
data class FlatMapA<A, B>(
    val sub: Async<A>,
    val f: (A) -> Async<B>,
) : Async<B>()


class TailRecTest {

    @Test
    fun normal() {
        val a = { x: Int -> x }
        val b = List(100000) { idx -> a }
            .fold(a) { ff, h -> { n: Int -> h(ff(n)) } }

        assertThrows<StackOverflowError>("expected") {
            b.invoke(1)
        }
    }

    @Test
    fun trampoline() {
        val f = { x: Int -> Return(x) }
        val g = List(100000) { idx -> f }
            .fold(f) { a: (Int) -> Tailrec<Int>, b: (Int) -> Tailrec<Int> ->
                { x: Int ->
                    //już gołe a(x) daje stackoverflow

                    Suspend { Unit }.flatMap { _ -> a(x).flatMap(b) }
                }
            }

        val result = run(g.invoke(1))

        assertThat(result, `is`(1))
    }

}