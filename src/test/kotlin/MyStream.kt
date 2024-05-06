import Option.Companion.filter
import Option.Companion.getOrElse
import Option.Companion.map
import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers
import org.junit.jupiter.api.Test

sealed class MyStream<out A> {
    companion object {
        fun <A> scons(hd: () -> A, tl: () -> MyStream<A>): MyStream<A> {
            val head: A by lazy(hd)
            val tail: MyStream<A> by lazy(tl)
            return SCons({ head }, { tail })
        }

        fun <A> empty(): MyStream<A> = Empty

        fun <A> of(vararg xs: A): MyStream<A> =
            when {
                xs.isEmpty() -> empty<A>()
                else -> scons({ xs[0] }, { of(*xs.copyOfRange(1, xs.size)) })
            }
    }


}

data class SCons<out A>(
    val head: () -> A,
    val tail: () -> MyStream<A>
) : MyStream<A>()

object Empty : MyStream<Nothing>()


fun <A> MyStream<A>.headOption(): Option<A> =
    when (this) {
        is Empty -> None
        is SCons -> Some(this.head())
    }

fun <A> MyStream<A>.toList(): MyList<A> =
    when (this) {
        is Empty -> Nil
        is SCons -> Cons(this.head(), this.tail().toList())
    }

fun <A> MyStream<A>.toList2(): MyList<A> {
    fun go(stream: MyStream<A>, acc: MyList<A>): MyList<A> =
        when (stream) {
            is Empty -> acc
            is SCons -> {
                go(stream.tail(), Cons(stream.head(), acc))
            }
        }

    return MyList.reverse(go(this, Nil))
}


fun <A> MyStream<A>.take(n: Int): MyStream<A> {
    //NOT tail recursion
    fun go(xs: MyStream<A>, n: Int): MyStream<A> = when (xs) {
        is Empty -> Empty
        is SCons ->
            if (n == 0) Empty
            else MyStream.scons(xs.head, { go(xs.tail(), n - 1) })
    }
    return go(this, n)
}


fun <A> MyStream<A>.drop(n: Int): MyStream<A> {
    tailrec fun go(xs: MyStream<A>, n: Int): MyStream<A> = when (xs) {
        is Empty -> Empty
        is SCons ->
            if (n == 0) xs
            else go(xs.tail(), n - 1)
    }
    return go(this, n)
}

fun <A> MyStream<A>.takeWhile(p: (A) -> Boolean): MyStream<A> {
    //NOT tail recursion
    fun go(xs: MyStream<A>): MyStream<A> = when (xs) {
        is Empty -> Empty
        is SCons -> {
            val met = p(xs.head())
            if (!met) Empty
            else MyStream.scons(xs.head) { go(xs.tail()) }
        }
    }
    return go(this)
}


fun <A, B> MyStream<A>.foldRight(
    z: () -> B,
    f: (A, () -> B) -> B
): B =
    when (this) {
        is SCons -> f(this.head()) {
            tail().foldRight(z, f)
        }

        is Empty -> z()
    }

fun <A> MyStream<A>.forAll(p: (A) -> Boolean): Boolean =
    foldRight({ true }) { elem, acc -> p(elem) && acc() }


fun <A, B> MyStream<A>.mapR(p: (A) -> B): MyStream<B> =
    foldRight({ Empty as MyStream<B> }) { elem, acc -> MyStream.scons({ p(elem) }, acc) }


fun <A> MyStream<A>.filter(p: (A) -> Boolean): MyStream<A> =
    foldRight({ Empty as MyStream<A> }) { elem, acc ->
        println("jest w srodku")
        if (p(elem)) {
            MyStream.scons({ elem }, acc)
        } else acc()
    }

fun <A> MyStream<A>.append(sa: () -> MyStream<A>): MyStream<A> =
    foldRight(sa) { elem, acc -> MyStream.scons({ elem }, acc) }

fun <A, B> MyStream<A>.flatMap(f: (A) -> MyStream<B>): MyStream<B> =
    foldRight({ Empty as MyStream<B> }) { elem, acc -> f(elem).append(acc) }

fun <A> MyStream<A>.takeWhile2(p: (A) -> Boolean): MyStream<A> {
    //TO jest zjebane
    return foldRight({ Empty as MyStream<A> }) { h, t -> if (p(h)) MyStream.scons({ h }, t) else t() }
}


fun ones(): MyStream<Int> = MyStream.scons({ 1 }, { ones() })

fun <A> constant(a: A): MyStream<A> = MyStream.scons({ a }, { constant(a) })


fun from(n: Int): MyStream<Int> = MyStream.scons({ n }, { from(n + 1) })


fun fibonacci(): MyStream<Int> {
    fun prod(elem1: Int, elem2: Int): MyStream<Int> = MyStream.scons({ elem1 }, { prod(elem2, elem1 + elem2) })
    return prod(0, 1)
}


fun <A, S> unfold(z: S, f: (S) -> Option<Pair<A, S>>): MyStream<A> {
    fun prod(state: S): MyStream<A> =
        f(state).map { MyStream.scons({ it.first }, { prod(it.second) }) }.getOrElse { Empty }

    return prod(z)
}


fun fibonacci2(): MyStream<Int> {
    val start: Pair<Int, Int> = Pair(0, 1)

    return unfold(start) { pair -> Some(Pair(pair.first, Pair(pair.second, pair.first + pair.second))) }
}

fun ones2(): MyStream<Int> = unfold(1) { Some(Pair(1, 1)) }

fun <A> constant2(a: A): MyStream<A> = unfold(a) { Some(Pair(a, a)) }


fun from2(n: Int): MyStream<Int> = unfold(n) { s -> Some(Pair(s, s + 1)) }


fun <A, B> MyStream<A>.map2(f: (A) -> B): MyStream<B> =
    unfold(this) { st ->
        st.headOption().map { Pair(f(it), (st as SCons<A>).tail()) }
    }

fun <A> MyStream<A>.takeWhile3(p: (A) -> Boolean): MyStream<A> =
    unfold(this) { stream -> stream.headOption().filter { p(it) }.map { Pair(it, (stream as SCons<A>).tail()) } }


fun <A, B, C> MyStream<A>.zipWith(
    that: MyStream<B>,
    f: (A, B) -> C
): MyStream<C> = unfold(Pair(this, that)) { streams ->
    val head1 = streams.first.headOption()
    val head2 = streams.second.headOption()
    Option.map2(head1, head2, f)
        .map { Pair(it, Pair((streams.first as SCons<A>).tail(), (streams.second as SCons<B>).tail())) }
}


fun <A, B> MyStream<A>.zipAll(
    that: MyStream<B>
): MyStream<Pair<Option<A>, Option<B>>> =
    unfold(Pair(this, that)) { (ths, tht) ->
        when (ths) {
            is SCons -> when (tht) {
                is SCons ->
                    Some(
                        Pair(
                            Pair(Some(ths.head()), Some(tht.head())),
                            Pair(ths.tail(), tht.tail())
                        )
                    )

                else ->
                    Some(
                        Pair(
                            Pair(Some(ths.head()), None),
                            Pair(ths.tail(), Empty)
                        )
                    )
            }

            else -> when (tht) {
                is SCons ->
                    Some(
                        Pair(
                            Pair(None, Some(tht.head())),
                            Pair(Empty, tht.tail())
                        )
                    )

                else -> None
            }
        }
    }

fun <A> MyStream<A>.startsWith(that: MyStream<A>): Boolean =
    this.zipAll(that).takeWhile { it.second != None }.forAll { it.first == it.second }


fun <A> MyStream<A>.tails(): MyStream<MyStream<A>> =
    unfold(this) { stream ->
        when (stream) {
            is Empty -> None
            is SCons -> Some(Pair(stream, stream.tail()))
        }
    }


class MyStreamTest {
    @Test
    fun `testTail`() {

        val myStream = SCons({ println("head"); 4 }, { Empty })

        myStream.headOption()
        myStream.headOption()
        //assertThat(MyList.tail(MyList.of(1, 2, 3)), Matchers.`is`(MyList.of(2, 3)))
    }

    @Test
    fun `testSconsSmartConstructor`() {

        val myStream = MyStream.scons({ println("head"); 4 }, { Empty })

        myStream.headOption()
        myStream.headOption()
        //assertThat(MyList.tail(MyList.of(1, 2, 3)), Matchers.`is`(MyList.of(2, 3)))
    }

    @Test
    fun `testOf`() {

        val myStream = MyStream.of(1, 2, 3)

        print(myStream.headOption())
        print((myStream as SCons<Int>).tail().headOption())
        print(((myStream as SCons<Int>).tail() as SCons<Int>).tail().headOption())

        print((((myStream as SCons<Int>).tail() as SCons<Int>).tail() as SCons<Int>).tail())
        //assertThat(MyList.tail(MyList.of(1, 2, 3)), Matchers.`is`(MyList.of(2, 3)))
    }

    @Test
    fun `testToList`() {

        val myStream = MyStream.of(1, 2, 3)

        assertThat(myStream.toList2(), Matchers.`is`(Cons(1, Cons(2, Cons(3, Nil)))))
    }

    @Test
    fun `testTake`() {

        val myStream = MyStream.of(1, 2, 3)

        assertThat(myStream.take(2).toList2(), Matchers.`is`(Cons(1, Cons(2, Nil))))
    }

    @Test
    fun `testDrop`() {

        val myStream = MyStream.of(1, 2, 3)

        assertThat(myStream.drop(2).toList2(), Matchers.`is`(Cons(3, Nil)))
    }

    @Test
    fun `testTakeWhile`() {

        val myStream = MyStream.of(1, 2, 3)


        val aaa = myStream.takeWhile2 {
            it == 2
        }
        println("huj" + aaa.toList())

//        assertThat(myStream.takeWhile2 {
//            it <= 2
//        }.toList2(), Matchers.`is`(Cons(1, Cons(2, Nil))))
    }


    @Test
    fun `testTakeWhile3`() {

        val myStream = MyStream.of(1, 2, 3)



        assertThat(myStream.takeWhile2 {
            it <= 2
        }.toList2(), Matchers.`is`(MyList.of(1, 2)))
    }

    @Test
    fun `testForALL`() {

        val myStream = MyStream.of(1, 2, 3)

        assertThat(myStream.forAll {
            it <= 3
        }, Matchers.`is`(true))
    }


    @Test
    fun `testForMap`() {
        val myStream = MyStream.of(1, 2, 3)
        assertThat(myStream.map2 {
            it * 2
        }.toList2(), Matchers.`is`(MyList.of(2, 4, 6)))
    }

    @Test
    fun `ss`() {
        val myStream = MyStream.of(1, 2, 3)
        val filter = myStream.filter {
            it > 1
        }
        println(filter)
    }

    @Test
    fun `testones`() {
        val ones = ones().take(3).toList()
        assertThat(ones, Matchers.`is`(MyList.of(1, 1, 1)))

        val con = constant(4).take(2).toList2()
        assertThat(con, Matchers.`is`(MyList.of(4, 4)))
    }


    @Test
    fun `testFrom`() {
        val xxx = from2(10).take(4).toList2()
        assertThat(xxx, Matchers.`is`(MyList.of(10, 11, 12, 13)))
    }

    @Test
    fun `testfibonacci`() {
        val xxx = fibonacci2().take(7).toList2()
        assertThat(xxx, Matchers.`is`(MyList.of(0, 1, 1, 2, 3, 5, 8)))
    }

    @Test
    fun `testunfold`() {
        val xxx = (unfold(1) { state -> Some(state to state) }).take(3).toList()
        assertThat(xxx, Matchers.`is`(MyList.of(1, 1, 1)))
    }

    @Test
    fun `testZipWith`() {
        val xxx = MyStream.of(1, 2, 3).zipWith(MyStream.of(10, 11, 12)) { a, b -> a + b }.toList2()
        assertThat(xxx, Matchers.`is`(MyList.of(11, 13, 15)))
    }


    @Test
    fun `testStartsWith`() {
        assertThat(MyStream.of(1, 2, 3).startsWith(MyStream.of(1, 2)), Matchers.`is`(true))
        assertThat(MyStream.of(1, 2, 3).startsWith(MyStream.of(1, 2, 3, 4)), Matchers.`is`(false))
        assertThat(MyStream.of(1, 2, 3).startsWith(MyStream.of(2)), Matchers.`is`(false))
    }

}