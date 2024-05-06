import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers
import org.junit.jupiter.api.Test

object Partial {
    fun <A, B, C> part(a: A, f: (a: A, b: B) -> C): (B) -> C {
        return fun(b: B): C {
            return f(a, b)
        }
    }

    fun <A, B, C> parti(a: A, f: (a: A, b: B) -> C): (B) -> C = { b: B -> f(a, b) }

}

object Curry {
    fun <A, B, C> curry(f: (A, B) -> C): (A) -> (B) -> C = { a: A ->
        { b: B -> f(a, b) }
    }

    fun <A, B, C> uncurry(f: (A) -> (B) -> C): (A, B) -> C = { a: A, b: B ->
        f(a)(b)
    }
}

object Compose {
    fun <A, B, C> compose(f: (A) -> B, g: (B) -> C): (A) -> C = { a: A -> g(f(a)) }

}

class PartialTest {
    @Test
    fun `test`() {
        val f = { a: Int, b: Int -> a + b }
        val my = Partial.part(1, f)
        val thier = Partial.parti(1, f)

        assertThat(thier(4), Matchers.`is`(5))
        assertThat(my(5), Matchers.`is`(6))
    }
}