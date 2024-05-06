import Either.Companion.flatMap
import arrow.core.firstOrNone
import arrow.core.getOrElse
import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers
import org.junit.jupiter.api.Test

sealed class Either<out E, out A> {
    companion object {
        fun <E, A, B> Either<E, A>.map(f: (A) -> B): Either<E, B> =
            when (this) {
                is Left -> this
                is Right -> Right(f(this.value))
            }


        fun <E, A, B> Either<E, A>.flatMap(f: (A) -> Either<E, B>): Either<E, B> =
            when (this) {
                is Left -> this
                is Right -> f(this.value)
            }


        fun <E, A> Either<E, A>.orElse(f: () -> Either<E, A>): Either<E, A> =
            when (this) {
                is Left -> f()
                is Right -> this
            }


        fun <E, A, B, C> map2(
            ae: Either<E, A>,
            be: Either<E, B>,
            f: (A, B) -> C,
        ): Either<E, C> = ae.flatMap { a -> be.map { b -> f(a, b) } }


        fun <A, B, E> traverse(list: List<A>, f: (A) -> Either<E, B>): Either<E, List<B>> {
            return list.fold(Right(emptyList<B>()) as Either<E, List<B>>) { acc, elem ->
                when (acc) {
                    is Left -> acc
                    is Right -> {
                        when (val newElem = f(elem)) {
                            is Left -> newElem
                            is Right -> Right(acc.value + newElem.value)
                        }
                    }
                }
            }
        }

        fun <E, A> sequence(list: List<Either<E, A>>): Either<E, List<A>> = traverse(list) { it }

    }

}

data class Left<out E>(val value: E) : Either<E, Nothing>()

data class Right<out A>(val value: A) : Either<Nothing, A>()

fun <A> catches(f: () -> A): Either<Exception, A> =
    try {
        Right(f())
    } catch (exc: Exception) {
        Left(exc)
    }


fun safeDiv(x: Int, y: Int): Either<Exception, Int> =
    try {
        Right(x / y)
    } catch (e: Exception) {
        Left(e)
    }

class EitherTest {
    @Test
    fun `testSafeDiv`() {
        assertThat(safeDiv(10, 2), Matchers.`is`(Right(5)))
    }

    @Test
    fun `testSafeDiv2`() {
        assertThat(safeDiv(10, 0), Matchers.instanceOf(Left::class.java))
    }

    @Test
    fun `testTraverse`() {
        assertThat(
            Either.traverse(listOf("1", "2", "3")) { str -> catches { str.toInt() } },
            Matchers.`is`(Right(listOf(1, 2, 3)))
        )
    }


    @Test
    fun `testTraverse2`() {
        val traverse = Either.traverse(listOf("1", "dupa", "fiut")) { str -> catches { str.toInt() } }
        assertThat(
            traverse,
            Matchers.instanceOf(Left::class.java)
        )
        val e: Left<Exception> = traverse as Left<Exception>
        assertThat(
            e.value.message, Matchers.containsString("dupa")
        )


    }


    //        https://medium.com/google-developer-experts/advanced-fp-for-the-enterprise-bee-traverse-b5e4e8b7b8e4
    @Test
    fun `exercise`() {


        val jvmProps = mapOf("prop1" to "value1", "prop2" to "value2")

        val mapFs = mapOf("file1" to "value1=foo\nvalue2=bar\nx=y")


        val dataMap = mapOf("foo" to "fooContent", "bar" to "barContent")

        fun getJVMProp(propName: String): Either<String, String> {
            val result = jvmProps[propName]
            return if (result != null) {
                Right(result)
            } else {
                Left("No JVM property: $propName")
            }
        }

        fun propertyViaFile(path: String, name: String): Either<String, String> {
            val propertyValue = { str: String -> str.substring(str.indexOf("=") + 1) }
            val content = mapFs[path]

            return if (content != null) {
                val lines = content.split("\n")

                lines
                    .firstOrNone { it.startsWith("$name=") }
                    .map(propertyValue)
                    .map { Right(it) as Either<String, String> }
                    .getOrElse { Left("No property called $name") }
            } else {
                 Left("No file called $path")
            }
        }

        fun readFile(path: String): Either<String, String> {
            val content = dataMap[path]
            return if (content != null) {
                Right(content)
            } else {
                Left("nie ma pliku $path")
            }
        }


        val fileContentsEither = Either.traverse(listOf("prop1", "prop2"), ::getJVMProp)
            .flatMap { propValues ->
                Either.traverse(propValues) { propValue -> propertyViaFile("file1", propValue) }
            }.flatMap { fileNames -> Either.traverse(fileNames, ::readFile) }


        assertThat(fileContentsEither, Matchers.`is`(Right(listOf("fooContent", "barContent"))))
    }


}