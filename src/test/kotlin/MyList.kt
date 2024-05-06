import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers
import org.junit.jupiter.api.Test

sealed class MyList<out A> {

    companion object {

        fun <A> of(vararg aa: A): MyList<A> {
            val tail = aa.sliceArray(1 until aa.size)
            return if (aa.isEmpty()) Nil else Cons(aa[0], of(*tail))
        }

        fun <A> reverse(input: MyList<A>): MyList<A> =
            foldLeft(input, Nil as MyList<A>) { acc, elem -> Cons(elem, acc) }


        fun <A> tail(xs: MyList<A>): MyList<A> = when (xs) {
            is Nil -> Nil
            is Cons -> xs.tail
        }

        fun <A> setHead(xs: MyList<A>, x: A): MyList<A> = when (xs) {
            is Nil -> Cons(x, xs)
            is Cons -> Cons(x, xs.tail)
        }

        fun <A> drop(l: MyList<A>, n: Int): MyList<A> {

            fun go(acc: MyList<A>, index: Int): MyList<A> {
                return when (acc) {
                    is Nil -> Nil
                    is Cons -> if (index == 0) acc else go(acc.tail, index - 1)
                }
            }

            return go(l, n)
        }

        fun <A> dropWhile(l: MyList<A>, f: (A) -> Boolean): MyList<A> {

            fun go(acc: MyList<A>): MyList<A> {
                return when (acc) {
                    is Nil -> Nil
                    is Cons -> if (f(acc.head)) go(acc.tail) else acc
                }
            }

            return go(l)
        }

        fun <A> init(l: MyList<A>): MyList<A> =
            when (l) {
                is Cons ->
                    if (l.tail == Nil) Nil
                    else Cons(l.head, init(l.tail))

                is Nil ->
                    throw IllegalStateException("Cannot init Nil list")

            }

        fun <A, B> foldRight(xs: MyList<A>, z: B, f: (A, B) -> B): B =
            when (xs) {
                is Nil -> z
                is Cons -> f(xs.head, foldRight(xs.tail, z, f))
            }

        fun <A> length(xs: MyList<A>): Int = foldRight(xs, 0) { _, b -> b + 1 }

        tailrec fun <A, B> foldLeft(xs: MyList<A>, z: B, f: (B, A) -> B): B =
            when (xs) {
                is Nil -> z
                is Cons -> foldLeft(xs.tail, f(z, xs.head), f)
            }

        fun <A> append(l1: MyList<A>, l2: MyList<A>): MyList<A> = foldRight(l1, l2) { elem, list -> Cons(elem, list) }
        fun <A> concatenate(l1: MyList<MyList<A>>): MyList<A> =
            foldRight(l1, Nil as MyList<A>) { l, acc -> append(l, acc) }

        fun addOne(l1: MyList<Int>): MyList<Int> = foldRight(l1, Nil as MyList<Int>) { l, acc -> Cons(l + 1, acc) }


        fun <A, B> map(xs: MyList<A>, f: (A) -> B): MyList<B> =
            foldRight(xs, Nil as MyList<B>) { elem, acc -> Cons(f(elem), acc) }


        fun <A> filter(xs: MyList<A>, f: (A) -> Boolean): MyList<A> =
            foldRight(xs, Nil as MyList<A>) { elem, acc -> if (f(elem)) Cons(elem, acc) else acc }

        fun <A, B> flatMap(xa: MyList<A>, f: (A) -> MyList<B>): MyList<B> =
            foldRight(xa, Nil as MyList<B>) { elem, acc -> append(f(elem), acc) }


        fun <A> filter2(xs: MyList<A>, f: (A) -> Boolean): MyList<A> =
            flatMap(xs) { elem ->
                if (f(elem)) Cons(elem, Nil)
                else Nil
            }

        fun <A, B, C> zipWith(l1: MyList<A>, l2: MyList<B>, f: (A, B) -> C): MyList<C> =
            when (l1) {
                is Nil -> Nil
                is Cons -> when (l2) {
                    is Nil -> Nil
                    is Cons -> Cons(f(l1.head, l2.head), zipWith(l1.tail, l2.tail, f))
                }
            }

        tailrec fun <A> hasSubsequence(xs: MyList<A>, sub: MyList<A>): Boolean =
            when (sub) {
                is Nil -> true
                is Cons -> {
                    val dropped = dropWhile(xs) { elem -> elem != sub.head }
                    when (dropped) {
                        is Nil -> false
                        is Cons -> hasSubsequence(dropped.tail, sub.tail)
                    }
                }
            }

    }


}


object Nil : MyList<Nothing>()

data class Cons<out A>(val head: A, val tail: MyList<A>) : MyList<A>()


class MyListTest {
    @Test
    fun `testTail`() {

        assertThat(MyList.tail(MyList.of(1, 2, 3)), Matchers.`is`(MyList.of(2, 3)))
    }

    @Test
    fun `testSetHead`() {

        assertThat(MyList.setHead(MyList.of(2, 2, 3), 1), Matchers.`is`(MyList.of(1, 2, 3)))
    }

    @Test
    fun `testDropN`() {

        assertThat(MyList.drop(MyList.of(2, 2, 3), 2), Matchers.`is`(MyList.of(3)))
    }

    @Test
    fun `testDropWhile`() {

        assertThat(MyList.dropWhile(MyList.of(1, 2, 3, 6, 9)) { a: Int -> a < 5 }, Matchers.`is`(MyList.of(6, 9)))
    }

    @Test
    fun `testInit`() {

        assertThat(MyList.init(MyList.of(1, 2, 3, 6, 9)), Matchers.`is`(MyList.of(1, 2, 3, 6)))
    }

    @Test
    fun `testLength`() {

        assertThat(MyList.length(MyList.of(1, 2, 3, 6, 9)), Matchers.`is`(5))
    }

    @Test
    fun `testFoldLeft`() {
        assertThat(MyList.foldLeft(MyList.of(1, 2, 3, 4, 5), 0) { a, b -> a + b }, Matchers.`is`(15))
    }

    @Test
    fun `testReverse`() {
        assertThat(
            MyList.foldLeft(MyList.of(1, 2, 3, 4, 5), Nil as MyList<Int>) { acc, elem -> Cons(elem, acc) },
            Matchers.`is`(MyList.of(5, 4, 3, 2, 1))
        )
    }

    @Test
    fun `testAppend`() {
        assertThat(
            MyList.append(MyList.of(1, 2, 3), MyList.of(4, 5, 6)),
            Matchers.`is`(MyList.of(1, 2, 3, 4, 5, 6))
        )
    }

    @Test
    fun `testConcatenate`() {
        assertThat(
            MyList.concatenate(MyList.of(MyList.of(1, 2, 3), MyList.of(4, 5, 6), MyList.of(7, 8, 9))),
            Matchers.`is`(MyList.of(1, 2, 3, 4, 5, 6, 7, 8, 9))
        )
    }

    @Test
    fun `testAddOne`() {
        assertThat(
            MyList.addOne(MyList.of(4, 5, 6)),
            Matchers.`is`(MyList.of(5, 6, 7))
        )
    }

    @Test
    fun `testMap`() {
        assertThat(
            MyList.map(MyList.of(4, 5, 6)) { a -> a * a },
            Matchers.`is`(MyList.of(16, 25, 36))
        )
    }

    @Test
    fun `testFilter`() {
        assertThat(
            MyList.filter(MyList.of(4, 5, 6, 7)) { a -> a % 2 == 0 },
            Matchers.`is`(MyList.of(4, 6))
        )
    }

    @Test
    fun `testFlatMap`() {
        assertThat(
            MyList.flatMap(MyList.of(1, 2, 3), { i -> MyList.of(i, i) }),
            Matchers.`is`(MyList.of(1, 1, 2, 2, 3, 3))
        )
    }

    @Test
    fun `testFilter2`() {
        assertThat(
            MyList.filter2(MyList.of(4, 5, 6, 7)) { a -> a % 2 == 0 },
            Matchers.`is`(MyList.of(4, 6))
        )
    }

    @Test
    fun `testZipWith`() {
        assertThat(
            MyList.zipWith(MyList.of(1, 2, 3, 7), MyList.of(9, 8, 7)) { a, b -> a + b },
            Matchers.`is`(MyList.of(10, 10, 10))
        )
    }

    @Test
    fun `testSubsuquence`() {
        assertThat(
            MyList.hasSubsequence(MyList.of(1, 2, 3, 7), MyList.of(1, 3, 7)),
            Matchers.`is`(true)
        )
    }
}