import arrow.Kind
import arrow.Kind2
import arrow.core.Either
import arrow.core.EitherOf
import arrow.core.EitherPartialOf
import arrow.core.ForListK
import arrow.core.ForOption
import arrow.core.ForSequenceK
import arrow.core.ListK
import arrow.core.ListKOf
import arrow.core.Option
import arrow.core.SequenceK
import arrow.core.Some
import arrow.core.extensions.EitherMonad
import arrow.core.fix
import arrow.core.getOrElse
import arrow.core.k
import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers
import org.junit.jupiter.api.Test

interface Functor<F> {
    fun <A, B> map(fa: Kind<F, A>, f: (A) -> B): Kind<F, B>

    fun <A, B> distribute(
        fab: Kind<F, Pair<A, B>>,
    ): Pair<Kind<F, A>, Kind<F, B>> = map(fab) { it.first } to map(fab) { it.second }

    fun <A, B> codistribute(
        e: arrow.core.Either<Kind<F, A>, Kind<F, B>>,
    ): Kind<F, arrow.core.Either<A, B>> = when (e) {
        is arrow.core.Either.Left -> map(e.value) { arrow.core.Either.Left(it) }
        is arrow.core.Either.Right -> map(e.value) { arrow.core.Either.Right(it) }
    }
}


val listFunctor = object : Functor<ForListK> {
    override fun <A, B> map(fa: ListKOf<A>, f: (A) -> B): ListKOf<B> = fa.fix().map(f)
}

val optionFunctor = object : Functor<ForOption> {
    override fun <A, B> map(fa: Kind<ForOption, A>, f: (A) -> B): Kind<ForOption, B> = fa.fix().map(f)
}


interface Monad<F> : Functor<F> {

    fun <A> unit(a: A): Kind<F, A>
    fun <A, B> flatMap(fa: Kind<F, A>, f: (A) -> Kind<F, B>): Kind<F, B>

    fun <A, B, C> map2(
        fa: Kind<F, A>,
        fb: Kind<F, B>,
        f: (A, B) -> C,
    ): Kind<F, C> = flatMap(fa) { a -> map(fb) { b -> f(a, b) } }

    override fun <A, B> map(fa: Kind<F, A>, f: (A) -> B): Kind<F, B> = flatMap(fa) { a -> unit(f(a)) }


    fun <A> sequence(lfa: List<Kind<F, A>>): Kind<F, List<A>> {
        val start: Kind<F, List<A>> = unit(emptyList())
        return lfa.foldRight(start) { elem, acc -> map2(elem, acc) { a, la -> listOf(a) + la } }
    }


    fun <A> sequenceAsTraverse(lfa: List<Kind<F, A>>): Kind<F, List<A>> = traverse(lfa) { it }


    //dwa razy przechodzę
    fun <A, B> traverse2(
        la: List<A>,
        f: (A) -> Kind<F, B>,
    ): Kind<F, List<B>> {
        return sequence(la.map(f))
    }

    fun <A, B> traverse(
        la: List<A>,
        f: (A) -> Kind<F, B>,
    ): Kind<F, List<B>> = la.foldRight(unit(emptyList<B>())) { a, acc -> map2(f(a), acc) { b, lb -> listOf(b) + lb } }


    fun <A> replicateM(n: Int, ma: Kind<F, A>): Kind<F, List<A>> {
        return if (n == 0) {
            unit(emptyList())
        } else {
            map2(ma, replicateM(n - 1, ma)) { elem, l -> listOf(elem) + l }
        }
    }

    fun <A> _replicateM(n: Int, ma: Kind<F, A>): Kind<F, List<A>> {
        val map: List<Kind<F, A>> = (1..n).map { ma }
        return sequence(map)
    }

    fun <A, B> product(
        ma: Kind<F, A>,
        mb: Kind<F, B>,
    ): Kind<F, Pair<A, B>> = map2(ma, mb) { a, b -> Pair(a, b) }


    fun <A> filterM(
        ms: List<A>,
        f: (A) -> Kind<F, Boolean>,
    ): Kind<F, List<A>> {

        return ms.foldRight(unit(emptyList())) { elem, acc ->
            val x: Kind<F, Boolean> = f(elem)
            flatMap(x) { b ->
                if (b) map(acc) { l -> listOf(elem) + l } else acc
            }
        }
    }

    //Ich implementacja
    fun <A> filterM2(
        ms: List<A>,
        f: (A) -> Kind<F, Boolean>,
    ): Kind<F, List<A>> = if (ms.isEmpty()) {
        unit(emptyList())
    } else {
        val tail = ms.subList(1, ms.size)
        flatMap(f(ms.first())) { succeed ->
            if (succeed) map(filterM2(tail, f)) { tail ->
                listOf(ms.first()) + tail
            } else filterM2(tail, f)
        }
    }

    fun <A, B, C> compose(
        f: (A) -> Kind<F, B>,
        g: (B) -> Kind<F, C>,
    ): (A) -> Kind<F, C> = { a: A -> flatMap(f(a), g) }


    fun <A> join(mma: Kind<F, Kind<F, A>>): Kind<F, A> = flatMap(mma) { it }


    fun <A, B> flatMapAsJoin(fa: Kind<F, A>, f: (A) -> Kind<F, B>): Kind<F, B> = join(map(fa) { f(it) })

    fun <A, B, C> composeAsJoin(
        f: (A) -> Kind<F, B>,
        g: (B) -> Kind<F, C>,
    ): (A) -> Kind<F, C> = { a: A -> join(map(f(a), g)) }
}

object Monads {

    fun optionMonad(): Monad<ForOption> = object : Monad<ForOption> {
        override fun <A> unit(a: A): Kind<ForOption, A> = Option.just(a)

        override fun <A, B> flatMap(fa: Kind<ForOption, A>, f: (A) -> Kind<ForOption, B>): Kind<ForOption, B> {
            return fa.fix().flatMap { a -> f(a).fix() }
        }
    }

    fun <E> eitherMonad() = object : EitherMonad<E> {
        //wszystko zaimplementowane w EitherMonad
    }


    fun listKMonad(): Monad<ForListK> = object : Monad<ForListK> {
        override fun <A> unit(a: A): Kind<ForListK, A> = ListK(listOf(a))

        override fun <A, B> flatMap(fa: Kind<ForListK, A>, f: (A) -> Kind<ForListK, B>): Kind<ForListK, B> {
            return fa.fix().flatMap { a -> f(a).fix() }
        }
    }

    fun sequenceKMonad(): Monad<ForSequenceK> = object : Monad<ForSequenceK> {
        override fun <A> unit(a: A): Kind<ForSequenceK, A> = SequenceK.just(a)

        override fun <A, B> flatMap(
            fa: Kind<ForSequenceK, A>,
            f: (A) -> Kind<ForSequenceK, B>,
        ): Kind<ForSequenceK, B> = fa.fix().flatMap(f)
    }
}


data class Id<out A>(val a: A) : IdOf<A> {
    companion object {
        fun <A> unit(a: A): Id<A> = Id(a)
    }

    fun <B> flatMap(f: (A) -> Id<B>): Id<B> = f(this.a)
    fun <B> map(f: (A) -> B): Id<B> = unit(f(this.a))
}

class ForId private constructor() {
    companion object
}

typealias IdOf<A> = Kind<ForId, A>

fun <A> IdOf<A>.fix() = this as Id<A>

fun idMonad() = object : Monad<ForId> {
    override fun <A> unit(a: A): IdOf<A> = Id.unit(a)

    override fun <A, B> flatMap(fa: IdOf<A>, f: (A) -> IdOf<B>): IdOf<B> = fa.fix().flatMap { a -> f(a).fix() }

    override fun <A, B> map(fa: IdOf<A>, f: (A) -> B): IdOf<B> = fa.fix().map(f)
}

sealed class ForState private constructor() {
    companion object
}

typealias StateOf<S, A> = Kind2<ForState, S, A>

typealias StatePartialOf<S> = Kind<ForState, S>

fun <S, A> StateOf<S, A>.fix() = this as State<S, A>

interface StateMonad<S> : Monad<StatePartialOf<S>> {

    override fun <A> unit(a: A): StateOf<S, A> = State { s -> a to s }

    override fun <A, B> flatMap(
        fa: StateOf<S, A>,
        f: (A) -> StateOf<S, B>,
    ): StateOf<S, B> = fa.fix().flatMap { a -> f(a).fix() }

}

val intStateMonad: StateMonad<Int> = object : StateMonad<Int> {}

val s1 = intStateMonad.unit(10)
val s2 = intStateMonad.unit(40)

fun replicateIntState(): StateOf<Int, List<Int>> = intStateMonad.replicateM(5, s1)


fun map2IntState(): StateOf<Int, Int> = intStateMonad.map2(s1, s2) { a, b -> a * b }


fun sequenceIntState(): StateOf<Int, List<Int>> = intStateMonad.sequence(listOf(s1, s2))


fun <A> zipWithIndex(la: List<A>): List<Pair<Int, A>> {
    val elemZero: Kind<Kind<ForState, Int>, List<Pair<Int, A>>> = intStateMonad.unit(emptyList<Pair<Int, A>>())
    val fold: Kind<Kind<ForState, Int>, List<Pair<Int, A>>> =
        la.fold(elemZero) { acc, a ->
            acc.fix().flatMap { xs ->
                acc.fix().get<Int>().flatMap { n ->
                    acc.fix().setState(n + 1).map { _ ->
                        listOf(n to a) + xs
                    }
                }
            }
        }
    val fix: State<Int, List<Pair<Int, A>>> = fold.fix()
    return fix.run(0).first.reversed()
}


class MonadTest {

    @Test
    fun `listFunctorTest`() {
        val some = listOf(1, 2).k()
        val mapped = listFunctor.map(some) { it + 3 }.fix()


        assertThat(mapped.toList(), Matchers.`is`(listOf(4, 5)))

    }


    @Test
    fun `optionFunctorTest`() {
        val some = arrow.core.Some(33)
        val mapped = optionFunctor.map(some) { it + 3 }.fix()
        val value = mapped.getOrElse { 0 }

        assertThat(value, Matchers.`is`(36))
    }

    @Test
    fun `distributeTest`() {

        val some = listOf(1, 2).k().map { i -> i to "value $i" }

        val (kind, kind2) = listFunctor.distribute(some)

        assertThat(kind.fix().toList(), Matchers.`is`(listOf(1, 2)))
        assertThat(kind2.fix().toList(), Matchers.`is`(listOf("value 1", "value 2")))
    }

    @Test
    fun `listMonadTest`() {

        val result = Monads.listKMonad().flatMap(listOf(1, 2).k()) { elem -> listOf(elem, elem, elem).k() }

        assertThat(result.fix().toList(), Matchers.`is`(listOf(1, 1, 1, 2, 2, 2)))
    }

    @Test
    fun `sequenceTest`() {

        val listOf: List<Option<Int>> = listOf(Option.just(1), Option.just(2))

        val result = Monads.optionMonad().sequence(listOf)

        assertThat(result.fix(), Matchers.`is`(Some(listOf(1, 2))))

    }

    @Test
    fun `traverseTest`() {
        val result = Monads.optionMonad().traverse(listOf(1, 2)) { e -> Option.just(e) }

        assertThat(result.fix(), Matchers.`is`(Some(listOf(1, 2))))

    }

    @Test
    fun `generateTest`() {
        val result = Monads.listKMonad().replicateM(3, listOf(1).k())

        assertThat(result.fix(), Matchers.`is`(listOf(listOf(1, 1, 1))))

    }

    @Test
    fun `filterMTest`() {
        val filterM1 = Monads.listKMonad().filterM(listOf(1, 2, 3, 4)) { el -> listOf(true, true).k() }
        val filterM2 = Monads.listKMonad().filterM2(listOf(1, 2, 3, 4)) { el -> listOf(true, true).k() }


        assertThat(filterM1.fix(), Matchers.`is`(filterM2.fix()))

    }


    @Test
    fun `checkOrder`() {
        data class Item(val name: String, val price: Double)
        data class Order(val item: Item, val quantity: Int)

        val simpleRNG = SimpleRNG(3)

        val genOrder: GenN<Order> = GenN.string().flatMap { name: String ->
            GenN.double(0..10).flatMap { price: Double ->
                GenN.choose(1, 100).map { quantity: Int ->
                    Order(Item(name, price), quantity)
                }
            }
        }


        val genItem: GenN<Item> = GenN.string().flatMap { name: String ->
            GenN.double(0..10).map { price: Double ->
                Item(name, price)
            }
        }


        val genOrder2: GenN<Order> = GenN.choose(1, 100).flatMap { quantity: Int ->
            genItem.map { item: Item ->
                Order(item, quantity)
            }
        }

        val (order, rng) = genOrder.sample.run(simpleRNG)
        val (order2, rng2) = genOrder2.sample.run(simpleRNG)

        println(order)
        println(order2)

    }

    @Test
    fun `checkIntStateReplicateM`() {

        val stateA = State.unit<Int, Int>(0).flatMap { j ->
            State { s -> Pair(j + s, s - 1) }
        }

        //5 razy wywołamy stateA ale przekazując mu za każdym razem stan będący
        // stanem wyjściowym poprzedniego wywołania
        val monad = intStateMonad.replicateM(5, stateA)
        val (result, newState) = monad.fix().run(10)
        assertThat(result, Matchers.`is`(listOf(10, 9, 8, 7, 6)))
        assertThat(newState, Matchers.`is`(5))

    }

    @Test
    fun `checkIntStateMap2`() {

        val stateA = State.unit<Int, Int>(0).flatMap { j ->
            State { s -> Pair(j + s, s - 1) }
        }

        val stateB = State.unit<Int, Int>(0).flatMap { j ->
            State { s -> Pair(j + s, s - 1) }
        }

        //wywołamy stateA  z danym stanem , wynikow stan będzie inputem dla stateB
        // w koncu zwrócę wartość
        val monad = intStateMonad.map2(stateA, stateB) { a, b -> a to b }
        val (result, newState) = monad.fix().run(10)

        assertThat(newState, Matchers.`is`(8))
        assertThat(result, Matchers.`is`(10 to 9))

    }

    @Test
    fun `testZipWithIndex`() {
        val result = zipWithIndex(listOf("a", "b", "c", "d"))

        assertThat(result, Matchers.`is`(listOf(Pair(0, "a"), Pair(1, "b"), Pair(2, "c"), Pair(3, "d"))))
    }


}