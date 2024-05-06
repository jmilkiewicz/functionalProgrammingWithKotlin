import org.junit.jupiter.api.Test
import java.util.concurrent.Callable
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit


fun <E> List<E>.splitAt(where: Int): Pair<List<E>, List<E>> {
    return if (where < this.size) {
        this.subList(0, where) to this.subList(where, this.size)
    } else {
        this.subList(0, this.size) to emptyList<E>()
    }

}

typealias Par<A> = (ExecutorService) -> Future<A>

fun <A> run(es: ExecutorService, a: Par<A>): Future<A> = a(es)

object Pars {
    fun <A> unit(a: A): Par<A> =
        { es: ExecutorService -> UnitFuture(a) }

    data class UnitFuture<A>(val a: A) : Future<A> {

        override fun get(): A = a

        override fun get(timeout: Long, timeUnit: TimeUnit): A = a

        override fun cancel(evenIfRunning: Boolean): Boolean = false

        override fun isDone(): Boolean = true

        override fun isCancelled(): Boolean = false
    }

    fun <A> lazyUnit(a: () -> A): Par<A> =
        fork { unit(a()) }

    fun <A, B, C> map2(
        a: Par<A>,
        b: Par<B>,
        f: (A, B) -> C
    ): Par<C> =
        { es: ExecutorService ->
            val af: Future<A> = a(es)
            val bf: Future<B> = b(es)
            UnitFuture(f(af.get(), bf.get()))
        }

    fun <A> fork(
        a: () -> Par<A>
    ): Par<A> =
        { es: ExecutorService ->
            es.submit(Callable<A> { a()(es).get() })
        }

    fun <A> delay(pa: () -> Par<A>): Par<A> =
        { es -> pa()(es) }


    fun <A> choice(cond: Par<Boolean>, t: Par<A>, f: Par<A>): Par<A> =
        { es: ExecutorService ->
            when (run(es, cond).get()) {
                true -> run(es, t)
                false -> run(es, f)
            }
        }

    fun <A> choiceN(n: Par<Int>, choices: List<Par<A>>): Par<A> =
        { es: ExecutorService ->
            val choiceIndex = run(es, n).get()
            choices[choiceIndex].invoke(es)
        }

    fun <A> choice2(cond: Par<Boolean>, t: Par<A>, f: Par<A>): Par<A> =
        choiceN(map(cond) { if (it) 0 else 1 }, listOf(t, f))

    fun <K, V> choiceMap(
        key: Par<K>,
        choices: Map<K, Par<V>>
    ): Par<V> = { es: ExecutorService ->
        val choiceKey = run(es, key).get()
        choices.getValue(choiceKey).invoke(es)
    }


    fun <A, B> flatMap(pa: Par<A>, choices: (A) -> Par<B>): Par<B> = { es: ExecutorService ->
        val choiceKey = run(es, pa).get()
        choices.invoke(choiceKey).invoke(es)
    }

    fun <A> join(a: Par<Par<A>>): Par<A> = flatMap(a) { it }


}


fun <A, B> asyncF(f: (A) -> B): (A) -> Par<B> = { a: A -> Pars.lazyUnit { f(a) } }


fun sortPar(parList: Par<List<Int>>): Par<List<Int>> = map(parList) { it.sorted() }


fun <A> sequence(ps: List<Par<A>>): Par<List<A>> =
    when {
        ps.isEmpty() -> Pars.unit(emptyList())
        ps.size == 1 -> map(ps.first()) { listOf(it) }
        else -> {
            val (l, r) = ps.splitAt(ps.size / 2)
            Pars.map2(sequence(l), sequence(r)) { la, lb ->
                la + lb
            }
        }
    }

fun <A> sequence2(ps: List<Par<A>>): Par<List<A>> =
    ps.foldRight(Pars.unit(emptyList<A>())) { elem, acc ->
        Pars.map2(
            elem,
            acc
        ) { va, l -> listOf(va) + l }
    }

fun <A, B> parMap(
    ps: List<A>,
    f: (A) -> B
): Par<List<B>> = Pars.fork {
    val fbs: List<Par<B>> = ps.map(asyncF(f))
    sequence(fbs)
}

fun <A> parFilter(
    sa: List<A>,
    f: (A) -> Boolean
): Par<List<A>> {
    val pars: List<Par<A>> = sa.map { Pars.lazyUnit { it } }
    val a: Par<List<A>> = sequence(pars)
    return map(a) { x -> x.filter { f(it) } }
}


fun <A, B> map(pa: Par<A>, f: (A) -> B): Par<B> = Pars.map2(pa, Pars.unit(Unit)) { el, _ -> f(el) }

class ParTest {

    @Test
    fun `testSimulateWithSingleCoin`() {

    }


}