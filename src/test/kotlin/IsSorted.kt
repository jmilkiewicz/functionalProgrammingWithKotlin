import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers.`is`
import org.junit.jupiter.api.Test

object Sorted {
    fun <T> isSorted(array: List<T>, order: (T, T) -> Boolean): Boolean {

        fun go(a: List<T>, elem: T): Boolean {
            return if (a.isEmpty()) {
                true
            } else {
                if (order(elem, a.first())) {
                    go(a.drop(1), a.first())
                } else {
                    false
                }
            }
        }

        return array.isEmpty() || go(array.drop(1), array.first())
    }
}

class IsSortedTest {
    @Test
    fun `test`() {
        assertThat(Sorted.isSorted(listOf(1, 2, 3, 4)) { a:Int, b:Int -> a <= b }, `is`(true))
    }

    @Test
    fun `test2`() {
        assertThat(Sorted.isSorted(listOf(1)) { a, b -> a > b }, `is`(true))
    }

    @Test
    fun `test3`() {
        assertThat(Sorted.isSorted(listOf(3, 2, 4)) { a, b -> a <= b }, `is`(false))
    }

    @Test
    fun `test4`() {
        assertThat(Sorted.isSorted(emptyList<Int>()) { a, b -> a <= b }, `is`(true))
    }

}