import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers
import org.hamcrest.Matchers.containsString
import org.hamcrest.Matchers.either
import org.hamcrest.Matchers.`is`
import org.junit.jupiter.api.Test

sealed class Choice {
    data class Letter(val letter: Char) : Choice()
    data object End : Choice()
}


class Dictionary(words: List<String>) {
    private val successors =
        words.flatMap { it.zipWithNext() }.groupBy({ it.first }, { it.second }).mapValues { it.value.toSet() }

    private val stopLetters = words.map { it.last() }.toHashSet()

    fun successorsOf(c: Char) = successors.getOrDefault(c, emptySet())
    fun isStopLetter(c: Char) = stopLetters.contains(c)

}

object Words2 {

    private fun generateWord(dictionary: Dictionary, accumulator: List<Char>, random: RngLimited): List<Char> {
        val lastLetter = accumulator.last()
        val successors = dictionary.successorsOf(lastLetter)
        if (successors.isEmpty()) {
            return accumulator
        } else {
            val successorsAsLetters = successors.map { Choice.Letter(it) }
            val options = if (dictionary.isStopLetter(lastLetter)) {
                successorsAsLetters + Choice.End
            } else {
                successorsAsLetters
            }

            val (optionIndex, newRandom) = random.fromZeroTill(options.size)
            return when (val option = options[optionIndex]) {
                is Choice.End -> accumulator
                is Choice.Letter -> generateWord(dictionary, accumulator.plus(option.letter), newRandom)
            }
        }
    }

    fun generate(input: List<String>, random: RngLimited): String {
        if(input.isEmpty() || input.all { it.isEmpty() }){
            return ""
        }
        val dictionary = Dictionary(input)

        val (index, newRandom) = random.fromZeroTill(input.size)
        val startLetter = input[index][0]
        return generateWord(dictionary, listOf(startLetter), newRandom).joinToString(separator = "")
    }

}


interface RngLimited {
    fun fromZeroTill(n: Int): Pair<Int, RngLimited>
}

data class SimpleRNGDecorator(val rng: RNG) : RngLimited {
    override fun fromZeroTill(n: Int): Pair<Int, RngLimited> {
        val (value, rng1) = nonNegativeLessThan(n).invoke(rng)
        return value to SimpleRNGDecorator(rng1)
    }
}

class TestWords2 {

    @Test
    fun `deterministic test for very simple input`() {
        val result = Words2.generate(listOf("ap", "ba"), SimpleRNGDecorator(SimpleRNG(3)))
        assertThat(
            result, either(`is`("ap")).or(`is`("ba")).or(`is`("bap")).or(`is`("a"))
        )
    }

    @Test
    fun `testProperty`() {
        val rng = SimpleRNG(2)


        val seedGenerator = genNonNegativeInt
        val input = genString(5).listOf().flatMap { strings -> seedGenerator.map { it to strings } }
        val result = forAllNS(input) { (seed, words) ->
            val result = Words2.generate(words, SimpleRNGDecorator(SimpleRNG(seed.toLong())))
            val chunks = result.windowed(2, 1, true)
            val inputJoined = words.joinToString("_")
            val allChunksExceptForLast = chunks.subList(0, chunks.size - 1)
            allChunksExceptForLast.forEach {
                assertThat(inputJoined, containsString(it))
            }
            val lastChunk = chunks.last()

            val lastLetters = words.map { it.last() }

            assertThat(lastChunk.last(), Matchers.`in`(lastLetters))
            true
        }.check(5, 10, rng)

        // maxSize - do ilu może wzrosnąć lista - 5+2,
        // testCaseSize - do wzoru ile razy wygenerować listę o danym rozmiarze
        assertThat(result, `is`(PassedN))
    }

}