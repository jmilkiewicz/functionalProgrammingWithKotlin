import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers.`is`
import org.junit.jupiter.api.Test

class StartTest {
    @Test
    fun `Adding 1 and 2 should be equal to 3`() {
        assertThat(1+2, `is`(3))
    }
}