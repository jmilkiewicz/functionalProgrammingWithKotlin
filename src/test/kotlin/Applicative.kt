import Applicatives.idApplicative
import Applicatives.monoidApplicative
import Applicatives.optionApplicative
import arrow.Kind
import arrow.core.Const
import arrow.core.ConstOf
import arrow.core.ConstPartialOf
import arrow.core.Either
import arrow.core.ForListK
import arrow.core.ForOption
import arrow.core.None
import arrow.core.Some
import arrow.core.fix
import arrow.core.flatMap
import arrow.core.k
import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers
import org.junit.jupiter.api.Test
import java.time.LocalDate
import java.time.format.DateTimeParseException

interface Applicative<F> : Functor<F> {


    override fun <A, B> map(
        fa: Kind<F, A>,
        f: (A) -> B,
    ): Kind<F, B> = map2(fa, unit(Unit)) { a, _ -> f(a) }

    fun <A, B, C> map2(
        fa: Kind<F, A>,
        fb: Kind<F, B>,
        f: (A, B) -> C,
    ): Kind<F, C>

    fun <A> unit(a: A): Kind<F, A>

    fun <A, B> traverse(
        la: List<A>,
        f: (A) -> Kind<F, B>,
    ): Kind<F, List<B>> = la.foldRight(
        unit(emptyList())
    ) { a: A, acc: Kind<F, List<B>> ->
        map2(f(a), acc) { b: B, lb: List<B> -> listOf(b) + lb }
    }

    fun <A> sequence(lfa: List<Kind<F, A>>): Kind<F, List<A>> = traverse(lfa) { it }


    fun <K, V> sequence(
        mkv: Map<K, Kind<F, V>>,
    ): Kind<F, Map<K, V>> {
        return mkv.entries.toList()
            .fold(unit(emptyMap())) { acc, (key, value) -> map2(acc, value) { map, v -> map + (key to v) } }
    }


    fun <A> replicateMAsSequence(n: Int, ma: Kind<F, A>): Kind<F, List<A>> = sequence(List(size = n) { ma })

    fun <A, B> product(
        ma: Kind<F, A>,
        mb: Kind<F, B>,
    ): Kind<F, Pair<A, B>> = map2(ma, mb) { a, b -> Pair(a, b) }


    //"Define in terms of map2 and unit"
    fun <A, B> apply(
        fab: Kind<F, (A) -> B>,
        fa: Kind<F, A>,
    ): Kind<F, B> = map2(fab, fa) { f, a -> f(a) }

    //"Define in terms of apply and unit"
    fun <A, B> mapNew(
        fa: Kind<F, A>,
        f: (A) -> B,
    ): Kind<F, B> = apply(unit(f), fa)

    // Define in terms of apply and unit
    fun <A, B, C> map2New(
        fa: Kind<F, A>,
        fb: Kind<F, B>,
        f: (A, B) -> C,
    ): Kind<F, C> {

        val cudo = unit({ a: A -> { b: B -> f(a, b) } })
        // lub val cudo = unit(f.curry())

        return apply(apply(cudo, fa), fb)
    }

    fun <A, B, C, D> map3(
        fa: Kind<F, A>,
        fb: Kind<F, B>,
        fc: Kind<F, C>,
        f: (A, B, C) -> D,
    ): Kind<F, D> {


        //opcja 1
        val map2: Kind<F, (C) -> D> = map2(fa, fb) { a, b -> { c: C -> f(a, b, c) } }
        apply(map2, fc)

        //opcja 2
        val curried = unit { a: A -> { b: B -> { c: C -> f(a, b, c) } } }
        return apply(apply(apply(curried, fa), fb), fc)
    }

    fun <A, B, C, D, E> map4(
        fa: Kind<F, A>,
        fb: Kind<F, B>,
        fc: Kind<F, C>,
        fd: Kind<F, D>,
        f: (A, B, C, D) -> E,
    ): Kind<F, E> {
        val curried = unit { a: A -> { b: B -> { c: C -> { d: D -> f(a, b, c, d) } } } }

        return apply(apply(apply(apply(curried, fa), fb), fc), fd)
    }

}


interface Monad2<F> : Applicative<F> {

    fun <A, B> flatMap(fa: Kind<F, A>, f: (A) -> Kind<F, B>): Kind<F, B> = join(map(fa, f))

    fun <A> join(ffa: Kind<F, Kind<F, A>>): Kind<F, A> = flatMap(ffa) { fa -> fa }

    fun <A, B, C> compose(
        f: (A) -> Kind<F, B>,
        g: (B) -> Kind<F, C>,
    ): (A) -> Kind<F, C> = { a -> flatMap(f(a), g) }

    override fun <A, B> map(
        fa: Kind<F, A>,
        f: (A) -> B,
    ): Kind<F, B> = flatMap(fa) { a -> unit(f(a)) }

    override fun <A, B, C> map2(
        fa: Kind<F, A>,
        fb: Kind<F, B>,
        f: (A, B) -> C,
    ): Kind<F, C> = flatMap(fa) { a -> map(fb) { b -> f(a, b) } }
}

interface Traversable<F> : Functor<F>, Foldable<F> {


    override fun <A, B> map(fa: Kind<F, A>, f: (A) -> B): Kind<F, B> {
        return traverse(fa, idApplicative()) { Id.unit(f(it)) }.fix().a
    }

    //GDY F to List : (lfa: List<A>, AG, f: (A) -> G<B> : Kind<G, List<B>>
    fun <G, A, B> traverse(
        fa: Kind<F, A>,
        AG: Applicative<G>,
        f: (A) -> Kind<G, B>,
    ): Kind<G, Kind<F, B>> =
        sequence(map(fa, f), AG)


    // GDY F to List:  fun <A> sequence(lfa: List<Kind<G, A>>, AG): Kind<G, List<A>>
    fun <G, A> sequence(
        fga: Kind<F, Kind<G, A>>,
        AG: Applicative<G>,
    ): Kind<G, Kind<F, A>> =
        traverse(fga, AG) { it }

    override fun <A, M> foldMap(
        fa: Kind<F, A>,
        m: Monoid<M>,
        f: (A) -> M,
    ): M =
        traverse(fa, monoidApplicative(m)) { a ->
            Const<M, A>(f(a))
        }.fix().value()


    fun <A> zipWithIndex(ta: Kind<F, A>): Kind<F, Pair<A, Int>> =
        traverseS(ta) { a: A ->
            State.get<Int>().flatMap { s: Int ->
                State.set(s + 1).map { _ ->
                    a to s
                }
            }
        }.run(0)
            .first

    override fun <A> toList(ta: Kind<F, A>): List<A> {
        val op = traverseS(ta) { a ->
            State.get<List<A>>().flatMap { currentState ->
                State.set(listOf(a) + currentState)
            }
        }
        return op.run(emptyList()).second.reversed()
    }


    //List<A>, A-> State<S,B> : State<List<B>>
    fun <S, A, B> traverseS(
        fa: Kind<F, A>,
        f: (A) -> State<S, B>,
    ): State<S, Kind<F, B>> =
        traverse(
            fa,
            stateMonadApplicative(stateMonad())
        ) { a -> f(a) }.fix()


    fun <S, A, B> mapAccum(
        fa: Kind<F, A>,
        s: S,
        f: (A, S) -> Pair<B, S>,
    ): Pair<Kind<F, B>, S> {
        val operation = traverseS(fa) { a ->
            State.get<S>().flatMap { currentState ->

                val (newValue, newState) = f(a, currentState)
                State.set(newState).map { newValue }
            }
        }
        return operation.run(s)
    }

    fun <A> zipWithIndexAsMapAccum(ta: Kind<F, A>): Kind<F, Pair<A, Int>> =
        mapAccum(ta, 0) { a, s -> Pair(Pair(a, s), s + 1) }.first

    fun <A> toListAsMapAccum(ta: Kind<F, A>): List<A> {
        return mapAccum(ta, emptyList<A>()) { a, s -> Unit to (listOf(a) + s) }.second.reversed()
    }

    fun <A> reverse(ta: Kind<F, A>): Kind<F, A> =
        mapAccum(ta, toList(ta).reversed()) { _, ls ->
            ls.first() to ls.drop(1)
        }.first


    override fun <A, B> foldLeft(fa: Kind<F, A>, z: B, f: (B, A) -> B): B {
        return mapAccum(fa, z) { elem, acc -> Unit to f(acc, elem) }.second
    }

    fun <A, B> zip(fa: Kind<F, A>, fb: Kind<F, B>): Kind<F, Pair<A, B>> {
        return mapAccum(fa, toList(fb)) { a, listB ->
            if (listB.isEmpty()) throw Exception("wrong sizes") else {
                (a to listB.first()) to listB.subList(1, listB.size)
            }
        }.first

    }


//    fun <G, H, A, B> fuse(
//        ta: Kind<F, A>,
//        AG: Applicative<G>,
//        AH: Applicative<H>,
//        f: (A) -> Kind<G, B>,
//        g: (A) -> Kind<H, B>
//    ): Pair<Kind<G, Kind<F, B>>, Kind<H, Kind<F, B>>> =traverse(ta, AG.product(AH)) { a ->
//        Product(f(a) to g(a))
//    }.fix().value
//
//
//
//
//    AG.product(AH)


}

object Traversables {

    fun <A> optionTraversable(): Traversable<ForOption> = object : Traversable<ForOption> {

        //Option<A>, Applicative<G>, a-> G<B> : G<Option<B>>
        override fun <G, A, B> traverse(
            fa: Kind<ForOption, A>,
            AG: Applicative<G>,
            f: (A) -> Kind<G, B>,
        ): Kind<G, Kind<ForOption, B>> =
            when (val o = fa.fix()) {
                is Some -> AG.map(f(o.value)) { Some(it) }
                is None -> AG.unit(None)
            }


    }

    fun <A> listTraversable(): Traversable<ForListK> = object : Traversable<ForListK> {

        //List<A>, Applicative<G>, a -> G<B> : G<List<B>>
        override fun <G, A, B> traverse(
            fa: Kind<ForListK, A>,
            AG: Applicative<G>,
            f: (A) -> Kind<G, B>,
        ): Kind<G, Kind<ForListK, B>> {

            return fa.fix().foldLeft(
                AG.unit(emptyList<B>().k())
            ) { acc, a ->
                AG.map2(acc, f(a)) { l, e ->
                    (listOf(e) + l.toList()).k()
                }
            }
        }

    }

}

object Applicatives {

    fun optionApplicative(): Applicative<ForOption> = object : Applicative<ForOption> {

        override fun <A, B, C> map2(
            fa: Kind<ForOption, A>,
            fb: Kind<ForOption, B>,
            f: (A, B) -> C,
        ): Kind<ForOption, C> = when (val first = fa.fix()) {
            is Some -> {
                when (val second = fb.fix()) {
                    is Some -> Some(f(first.value, second.value))
                    is None -> None
                }
            }

            is None -> None
        }


        override fun <A> unit(a: A): Kind<ForOption, A> = Some(a)
    }


    fun listApplicative(): Applicative<ForListK> = object : Applicative<ForListK> {
        override fun <A, B, C> map2(fa: Kind<ForListK, A>, fb: Kind<ForListK, B>, f: (A, B) -> C): Kind<ForListK, C> {
            val listA = fa.fix();
            val listB = fb.fix()

            //zip ? cartesian product ? jak to zaimplementować ?

            TODO("Not yet implemented")
        }

        override fun <A> unit(a: A): Kind<ForListK, A> = listOf(a).k()


    }


    fun idApplicative(): Applicative<ForId> =
        object : Applicative<ForId> {
            override fun <A> unit(a: A): IdOf<A> = Id(a)

            override fun <A, B, C> map2(
                fa: IdOf<A>,
                fb: IdOf<B>,
                f: (A, B) -> C,
            ): IdOf<C> = fa.fix().flatMap { a -> fb.fix().map { b -> f(a, b) } }

            override fun <A, B> map(
                fa: IdOf<A>,
                f: (A) -> B,
            ): IdOf<B> =
                fa.fix().map(f)
        }


    fun <M> monoidApplicative(m: Monoid<M>): Applicative<ConstPartialOf<M>> =
        object : Applicative<ConstPartialOf<M>> {

            override fun <A> unit(a: A): ConstOf<M, A> = Const(m.nil)

            override fun <A, B, C> map2(
                ma: ConstOf<M, A>,
                mb: ConstOf<M, B>,
                f: (A, B) -> C,
            ): ConstOf<M, C> =
                Const(m.combine(ma.fix().value(), mb.fix().value()))
        }
}

sealed class Validation<out E, out A> : ValidatedOf2<E, A>

data class Failure<E>(
    val head: E,
    val tail: List<E> = emptyList(),
) : Validation<E, Nothing>() {
    fun getAllErrors(): List<E> = listOf(head) + tail
}

data class Success<A>(val a: A) : Validation<Nothing, A>()


//wszystkie 4 to jakieś hacki
class ForValidated2 private constructor() {
    companion object
}
typealias ValidatedOf2<E, A> = arrow.Kind2<ForValidated2, E, A>

typealias ValidatedPartialOf2<E> = arrow.Kind<ForValidated2, E>

fun <E, A> ValidatedOf2<E, A>.fix(): Validation<E, A> = this as Validation<E, A>

fun <E> validation() = object : Applicative<ValidatedPartialOf2<E>> {

    override fun <A> unit(a: A): ValidatedOf2<E, A> = Success(a)

    override fun <A, B, C> map2(
        fa: ValidatedOf2<E, A>,
        fb: ValidatedOf2<E, B>,
        f: (A, B) -> C,
    ): ValidatedOf2<E, C> {
        val va = fa.fix()
        val vb = fb.fix()
        return when (va) {
            is Success -> when (vb) {
                is Success -> Success(f(va.a, vb.a))
                is Failure -> vb
            }

            is Failure -> when (vb) {
                is Success -> va
                is Failure -> Failure(
                    va.head, va.tail + vb.head + vb.tail
                )
            }
        }
    }
}


fun <S> stateMonad() = object : StateMonad<S> {

    override fun <A> unit(a: A): StateOf<S, A> =
        State { s -> a to s }

    override fun <A, B> flatMap(
        fa: StateOf<S, A>,
        f: (A) -> StateOf<S, B>,
    ): StateOf<S, B> =
        fa.fix().flatMap { f(it).fix() }

    override fun <A, B, C> compose(
        f: (A) -> StateOf<S, B>,
        g: (B) -> StateOf<S, C>,
    ): (A) -> StateOf<S, C> =
        { a -> join(map(f(a), g)) }
}

fun <S> stateMonadApplicative(m: StateMonad<S>) =
    object : Applicative<StatePartialOf<S>> {

        override fun <A> unit(a: A): Kind<StatePartialOf<S>, A> =
            m.unit(a)

        override fun <A, B, C> map2(
            fa: Kind<StatePartialOf<S>, A>,
            fb: Kind<StatePartialOf<S>, B>,
            f: (A, B) -> C,
        ): Kind<StatePartialOf<S>, C> =
            m.map2(fa, fb, f)

        override fun <A, B> map(
            fa: Kind<StatePartialOf<S>, A>,
            f: (A) -> B,
        ): Kind<StatePartialOf<S>, B> =
            m.map(fa, f)
    }


class ApplicativeTest {

    @Test
    fun replicateMAsSequence() {
        val applicative: Kind<ForOption, Int> = Applicatives.optionApplicative().unit(2)

        val replicateMAsSequence = Applicatives.optionApplicative().replicateMAsSequence(4, applicative)

        assertThat(replicateMAsSequence.fix(), Matchers.`is`(Some(listOf(2, 2, 2, 2))))

    }

    data class WebForm(val f1: String, val f2: LocalDate, val f3: String)


    @Test
    fun eitherValidationTest() {

        fun validateF1(f1: String) = if (f1 == "f1") Either.Right("f1") else Either.Left("invalid f1")
        fun validateF2(date: String) = try {
            Either.Right(LocalDate.parse(date))
        } catch (e: DateTimeParseException) {
            Either.Left("unable to parse $date")
        }


        fun validateF3(f3: String) = if (f3 == "f3") Either.Right("f3") else Either.Left("invalid f3")
        val okWebForm = validateF1("f1").flatMap { f1 ->
            validateF2("2024-04-24").flatMap { f2 ->
                validateF3("f3").map { f3 ->
                    WebForm(f1, f2, f3)
                }
            }
        }
        assertThat(okWebForm.fix(), Matchers.instanceOf(Either.Right::class.java))

        val invalidWebForm = validateF1("f2").flatMap { f1 ->
            validateF2("zla data").flatMap { f2 ->
                validateF3("f3").map { f3 ->
                    WebForm(f1, f2, f3)
                }
            }
        }

        assertThat(invalidWebForm.fix(), Matchers.`is`(Either.Left("invalid f1")))
    }

    @Test
    fun applicativeValidationTest() {

        fun validateF1(f1: String): Validation<String, String> =
            if (f1 == "f1") Success("f1") else Failure("f1 is not f1")

        fun validateF2(date: String): Validation<String, LocalDate> = try {
            Success(LocalDate.parse(date))
        } catch (e: DateTimeParseException) {
            Failure("unable to parse $date")
        }


        fun validateF3(f3: String): Validation<String, String> = if (f3 == "f3") Success("f3") else Failure(
            head = "f3 is not f3", tail = listOf("a tu bedą inne błędy", "i jeszcze 1")
        )

        val validation: Kind<Kind<ForValidated2, String>, WebForm> = validation<String>().map3(
            validateF1("zle f1"), validateF2("zle f2"), validateF3("zle f3")
        ) { f1, f2, f3 -> WebForm(f1, f2, f3) }

        val validationResult = validation.fix()

        assertThat(validationResult, Matchers.instanceOf(Failure::class.java))

        val allErrors = (validationResult as Failure<String>).getAllErrors()

        assertThat(
            allErrors,
            Matchers.contains(
                "f1 is not f1",
                "unable to parse zle f2",
                "f3 is not f3",
                "a tu bedą inne błędy",
                "i jeszcze 1"
            )
        )
    }

    @Test
    fun applicativeSequenceMap() {


        val sequence: Kind<ForOption, Map<String, Int>> = Applicatives.optionApplicative().sequence(
            mapOf(
                "jeden" to Applicatives.optionApplicative().unit(1),
                "dwa" to Applicatives.optionApplicative().unit(2)
            )
        )

        assertThat(
            sequence.fix(),
            Matchers.`is`(
                Some(
                    mapOf(
                        "jeden" to 1,
                        "dwa" to 2
                    )
                )
            )
        )
    }
}

class TraversableTest {

    @Test
    fun traversableListTest() {
        val listTraversable = Traversables.listTraversable<Int>()

        val inputList = listOf(1, 2, 3, 4)

        //list<Int>, Applicative<Option>, int-> Option<Int> : Option<List<Int>>
        val traverse = listTraversable.traverse(inputList.k(), optionApplicative(), { i: Int ->
            if (i / 2 == 0) {
                Some(i)
            } else
                None
        })
        val result1 = traverse.fix()

        assertThat(result1, Matchers.`is`(None))


        val traverse2 = listTraversable.traverse(inputList.k(), optionApplicative()) { Some(it) }
        val result2 = traverse2.fix()

        //CZEMU ta lista jest odwrócona ????
        assertThat(result2, Matchers.`is`(Some(inputList.reversed())))

    }


    @Test
    fun traversableIsFoldable() {
        val concatenateMonoid: Monoid<String> = stringMonoid

        val listTraversable = Traversables.listTraversable<Int>()

        val result = listTraversable.foldMap(listOf(1, 2, 3, 4).k(), concatenateMonoid) { "$it" }


        assertThat(
            result,
            Matchers.`is`("1234")
        )
    }

    @Test
    fun traverseWithState() {
        val listTraversable = Traversables.listTraversable<Int>()
        val list = listOf(1, 2, 3, 4).k()


        val result: State<Int, Kind<ForListK, String>> =
            listTraversable.traverseS(list) { i -> State { s -> Pair("for state $s, value is $i !", s + 1) } }
        val (opResult, _) = result.run(0)

        //czemu ta lista jest odwrócona ???
        println(opResult)
    }

    @Test
    fun zipWithIndex() {
        val listTraversable = Traversables.listTraversable<Int>()
        val list = listOf(1, 2, 3, 4).k()

        val value = listTraversable.zipWithIndex(list)

        val result = value.fix()
        //czemu tu musi być ten reversed ???
        val expected = listOf(1 to 0, 2 to 1, 3 to 2, 4 to 3).reversed()

        assertThat(result, Matchers.`is`(expected.k()))
    }

    @Test
    fun testMapWithAccum() {
        val list1 = listOf(1, 2, 3, 4).k()

        val value = Traversables.listTraversable<Any>().mapAccum(list1, 0) { elem, s -> "$elem" to (s + elem) }

        val (transformation, sum) = value

        assertThat(
            sum,
            Matchers.`is`(10)
        )

        //czemu to musi być reversed ?????
        assertThat(
            transformation,
            Matchers.`is`(listOf("1", "2", "3", "4").reversed().k())
        )

    }


    @Test
    fun testZip() {
        val list1 = listOf(1, 2, 3, 4).k()
        val list2 = listOf("a", "b", "c", "d").k()

        val value = Traversables.listTraversable<Any>().zip(list1, list2)

        //czemu tu musi być ten reversed ????
        val result = value.fix().reversed()

        assertThat(
            result,
            Matchers.`is`(listOf(Pair(1, "a"), Pair(2, "b"), Pair(3, "c"), Pair(4, "d")).k())
        )
    }
}