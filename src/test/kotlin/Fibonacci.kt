import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers.`is`
import org.junit.jupiter.api.Test

object Fibonacci {
    fun generate(n: Int): Int {
        fun go(l: Int, e1: Int, e2: Int): Int {
            if (l == n) {
                return e1
            }
            return go(l + 1, e2, e1 + e2)
        }

        if (n == 0 || n == 1) {
            return n
        } else {
            return go(0, 0, 1)
        }


    }
}

class FibonacciTest {
    @Test
    fun `test`() {
        assertThat(Fibonacci.generate(4), `is`(3))
    }

    @Test
    fun `test2`() {
        assertThat(Fibonacci.generate(11), `is`(89))
    }
}