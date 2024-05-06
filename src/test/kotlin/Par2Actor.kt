import org.junit.jupiter.api.Test
import java.util.concurrent.Callable
import java.util.concurrent.CountDownLatch
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.Future
import java.util.concurrent.atomic.AtomicReference

abstract class FutureC<A> {
    internal abstract fun invoke(cb: (A) -> Unit)
}

typealias Par2Actor<A> = (ExecutorService) -> FutureC<A>


fun <A> run(es: ExecutorService, pa: Par2Actor<A>): A {
    val ref = AtomicReference<A>()
    val latch = CountDownLatch(1)
    pa(es).invoke { a: A ->
        ref.set(a)
        latch.countDown()
    }
    latch.await()
    return ref.get()
}

object ParActor {
    fun <A> unit(a: A): Par2Actor<A> = { es: ExecutorService ->
        object : FutureC<A>() {
            override fun invoke(cb: (A) -> Unit) {
                cb(a)
            }
        }
    }

    fun <A> lazyUnit(a: () -> A): Par2Actor<A> = forkA { unit(a()) }

    fun <A, B> asyncF(f: (A) -> B): (A) -> Par2Actor<B> = { a: A ->
        lazyUnit { f(a) }
    }

    fun <A> delay(pa: () -> Par<A>): Par<A> =
        { es -> pa()(es) }


    fun <A, B> map(pa: Par2Actor<A>, f: (A) -> B): Par2Actor<B> = TODO()

//    fun <A, B> chooser(pa: Par2Actor<A>, choices: (A) -> Par2Actor<B>): Par2Actor<B> = {executorService ->
//        val future: FutureC<A> = pa.invoke(executorService)
//        future.run {  }
//    }



    fun <A, B, C> map22(
        a: Par2Actor<A>, b: Par2Actor<B>, f: (A, B) -> C
    ): Par<C> = TODO()


}


fun <A> forkA(a: () -> Par2Actor<A>): Par2Actor<A> =
    { es: ExecutorService ->
        object : FutureC<A>() {
            override fun invoke(cb: (A) -> Unit) =
                eval(es) { a()(es).invoke(cb) }
        }
    }

fun eval(es: ExecutorService, r: () -> Unit) {
    es.submit(Callable { r() })
}


class Par2ActorTest {


    @Test
    fun `lawsFork`() {
        val newFixedThreadPool = Executors.newFixedThreadPool(1);

        val es = Executors.newFixedThreadPool(1)

        val a: Par<Int> = Pars2.lazyUnit { 42 + 1 }
        val b: Par<Int> = fork { a }
        a.invoke(newFixedThreadPool).get()
        b.invoke(newFixedThreadPool).get()
    }


}