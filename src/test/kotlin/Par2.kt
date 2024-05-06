import Pars2.unit
import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers
import org.junit.jupiter.api.Test
import java.util.concurrent.Callable
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit

typealias Par2<A> = (ExecutorService) -> Future<A>


object Pars2 {
    fun <A> unit(a: A): Par2<A> = { es: ExecutorService -> UnitFuture(a) }

    fun <A> lazyUnit(a: () -> A): Par2<A> = fork { unit(a()) }

    fun <A, B> asyncF(f: (A) -> B): (A) -> Par<B> = { a: A ->
        lazyUnit { f(a) }
    }

    fun <A> delay(pa: () -> Par<A>): Par<A> =
        { es -> pa()(es) }

    fun sortPar(parList: Par2<List<Int>>): Par2<List<Int>> = map(parList) { it.sorted() }

    fun <A, B> map(pa: Par<A>, f: (A) -> B): Par<B> = map22(pa, unit(Unit), { a, _ -> f(a) })

    fun <A, B> parMap(
        ps: List<A>, f: (A) -> B
    ): Par<List<B>> = fork {
        val asyncF: (A) -> Par<B> = asyncF(f)

        val map: List<Par2<B>> = ps.map { asyncF(it) }
        sequence(map)
    }

    fun parMax(
        ps: List<Int>,
    ): Par<Int> {
        return if (ps.size <= 2) {
            Pars.unit(ps.max())
        } else {
            val (left, right) = ps.splitAt(ps.size / 2)
            val leftP = fork { parMax(left) }
            val rightP = fork { parMax(right) }
            map22(leftP, rightP) { a, b -> listOf(a, b).max() }
        }
    }


    //To jest implementacja z książki - nie jest parallel !!!
    fun <A> parFilter(
        sa: List<A>, f: (A) -> Boolean
    ): Par<List<A>> {
        val pars: List<Par<A>> = sa.map { lazyUnit { it } }
        return map(sequence(pars)) { la -> la.flatMap { if (f(it)) listOf(it) else emptyList() } }
    }


    fun <A> parFilter2(
        sa: List<A>, f: (A) -> Boolean
    ): Par<List<A>> {
        val pars: List<Par<Pair<A, Boolean>>> = sa.map { lazyUnit { it to f(it) } }


        //Sequence ale z filtrowaniem boolean values
        return pars.foldRight(unit(emptyList())) { elem, acc ->
            map22(elem, acc) { e: Pair<A, Boolean>, l: List<A> ->
                if (e.second) {
                    listOf(e.first) + l
                } else l
            }
        }
    }


    fun <A, B> sequentialMap(
        ps: List<A>, f: (A) -> B
    ): Par<List<B>> {

        val map: List<Par2<B>> = ps.map { unit(f(it)) }
        return sequence(map)
    }


    fun <A> sequence(ps: List<Par<A>>): Par<List<A>> = ps.foldRight(unit(emptyList())) { elem, acc ->
        map22(elem, acc) { e, l -> listOf(e) + l }
    }


    data class UnitFuture<A>(val a: A) : Future<A> {

        override fun get(): A = a

        override fun get(timeout: Long, timeUnit: TimeUnit): A = a

        override fun cancel(evenIfRunning: Boolean): Boolean = false

        override fun isDone(): Boolean = true

        override fun isCancelled(): Boolean = false
    }

    fun <A, B, C> map22(
        a: Par<A>, b: Par<B>, f: (A, B) -> C
    ): Par<C> = { es: ExecutorService ->
        val af: Future<A> = a(es)
        val bf: Future<B> = b(es)
        UnitFuture(f(af.get(), bf.get()))
    }

}


fun <A, B, C> map2(a: Par2<A>, b: Par2<B>, f: (A, B) -> C): Par2<C> = { executorService ->
    val futureA = a.invoke(executorService)
    val futureB = b.invoke(executorService)
    Pars2.UnitFuture(f(futureA.get(), futureB.get()))

}

fun <A> fork(a: () -> Par2<A>): Par2<A> = { executorService ->
    executorService.submit(Callable { a().invoke(executorService).get() })
}

fun sum2(ints: List<Int>): Par2<Int> = if (ints.size <= 1) Pars2.lazyUnit { ints.firstOrNull() ?: 0 }
else {
    val (l, r) = ints.splitAt(ints.size / 2)
    val result = map2(sum2(l), sum2(r)) { lx: Int, rx: Int -> lx + rx }
    result

}

fun sum3(ints: List<Int>): Par2<Int> = if (ints.size <= 1) Pars2.lazyUnit { ints.firstOrNull() ?: 0 }
else {
    val (l, r) = ints.splitAt(ints.size / 2)
    map2(fork { sum3(l) }, fork { sum3(r) }) { lx: Int, rx: Int -> lx + rx }
}


class Par2Test {


    @Test
    fun `unitIsEager`() {
        val x = mutableListOf<Int>()

        unit {
            x.add(333)
            x[0]
        }
        assertThat(x, Matchers.`is`(listOf(333)))
    }


    @Test
    fun `parMap`() {

        val newFixedThreadPool = Executors.newFixedThreadPool(10);
        val execution = Pars2.parMap(listOf(1, 2, 3, 4)) { v ->
            println("wszedłem dla $v")
            Thread.sleep(3000)
            println("wychodzę dla $v")
            v
        }.invoke(newFixedThreadPool)

        assertThat(execution.get(), Matchers.`is`(listOf(1, 2, 3, 4)))

    }

    @Test
    fun `seqMap`() {

        val newFixedThreadPool = Executors.newFixedThreadPool(10);
        val execution = Pars2.sequentialMap(listOf(1, 2, 3, 4)) { v ->
            println("wszedłem dla $v")
            Thread.sleep(1000)
            println("wychodzę dla $v")
            v
        }.invoke(newFixedThreadPool)

        assertThat(execution.get(), Matchers.`is`(listOf(1, 2, 3, 4)))

    }


    @Test
    fun `parFilter`() {

        val newFixedThreadPool = Executors.newFixedThreadPool(10);
        val execution = Pars2.parFilter(listOf(1, 2, 3, 4)) { v ->
            println("wszedłem dla $v")
            Thread.sleep(1000)
            println("wychodzę dla $v")
            v < 10
        }.invoke(newFixedThreadPool)

        assertThat(execution.get(), Matchers.`is`(listOf(1, 2, 3, 4)))

    }

    @Test
    fun `parFilter2`() {

        val newFixedThreadPool = Executors.newFixedThreadPool(10);
        val execution = Pars2.parFilter2(listOf(1, 2, 3, 4)) { v ->
            println("wszedłem dla $v")
            Thread.sleep(2000)
            println("wychodzę dla $v")
            v < 2
        }.invoke(newFixedThreadPool)

        assertThat(execution.get(), Matchers.`is`(listOf(1)))

    }

    @Test
    fun `parMax`() {

        val newFixedThreadPool = Executors.newFixedThreadPool(10);
        val execution = Pars2.parMax(listOf(1, 2, 3, 4,3,5,31,4,422,434,3,3,13,33,12,4000)).invoke(newFixedThreadPool)

        assertThat(execution.get(), Matchers.`is`(4000))

    }

    @Test
    fun `lawsFork`() {
        val newFixedThreadPool = Executors.newFixedThreadPool(1);


        val a: Par<Int> = Pars2.lazyUnit { 42 + 1 }
        val b: Par<Int> = fork { a }
        a.invoke(newFixedThreadPool).get()
        b.invoke(newFixedThreadPool).get()
    }


}