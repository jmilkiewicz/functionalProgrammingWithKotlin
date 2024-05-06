import org.hamcrest.MatcherAssert.assertThat
import org.hamcrest.Matchers
import org.junit.jupiter.api.Test

sealed class Input

object Coin : Input()
object Turn : Input()

data class Machine(
    val locked: Boolean,
    val candies: Int,
    val coins: Int
)


sealed class Output
object Candy : Output()
object NoAction : Output()


fun simulateMachine(inputs: List<Input>): State<Machine, List<Output>> {
    val map: List<State<Machine, Output>> = inputs.map { i: Input ->
        (State { ls: Machine ->
            when {
                ls.candies == 0 -> NoAction to ls
                i is Coin -> if (ls.locked) {
                    NoAction to ls.copy(locked = false, coins = ls.coins + 1)
                } else {
                    NoAction to ls
                }

                i is Turn -> if (ls.locked) {
                    NoAction to ls
                } else {
                    Candy to ls.copy(locked = true, candies = ls.candies - 1)
                }

                else -> {
                    println("nie powinnnienem tu wejsc")
                    NoAction to ls
                }
            }
        })
    }
    return State.sequence(map)
}

fun simulateMachine2(inputs: List<Input>): State<Machine, Machine> = State { state ->
    val map: List<State<Machine, Output>> = inputs.map { i: Input ->
        (State { ls: Machine ->
            when {
                ls.candies == 0 -> NoAction to ls
                i is Coin -> if (ls.locked) {
                    NoAction to ls.copy(locked = false, coins = ls.coins + 1)
                } else {
                    NoAction to ls
                }

                i is Turn -> if (ls.locked) {
                    NoAction to ls
                } else {
                    Candy to ls.copy(locked = true, candies = ls.candies - 1)
                }

                else -> {
                    println("nie powinnnienem tu wejsc")
                    NoAction to ls
                }
            }
        })
    }

    //zamiast tego super byłoby użyć traversable mapAccum
    val sequence = State.sequence(map)

    sequence.get<Machine>().run(state)
}


class MachineTest {
    val myMachine = Machine(true, 5, 10)

    @Test
    fun `testSimulateWithSingleCoin`() {
        val simulateMachine = simulateMachine(listOf(Coin))
        val (outputs, machine) = simulateMachine.run(myMachine)

        assertThat(outputs, Matchers.`is`(listOf(NoAction)))
        assertThat(machine, Matchers.`is`(myMachine.copy(locked = false, coins = myMachine.coins + 1)))
    }

    @Test
    fun `testSimulateWithSingleKnob`() {
        val simulateMachine = simulateMachine(listOf(Turn))
        val (outputs, machine) = simulateMachine.run(myMachine)

        assertThat(outputs, Matchers.`is`(listOf(NoAction)))
        assertThat(machine, Matchers.`is`(myMachine))
    }


    @Test
    fun `testCoinOnUnLockedMachine`() {
        val unlockedMachine = myMachine.copy(locked = false)
        val simulateMachine = simulateMachine(listOf(Coin))
        val (outputs, machine) = simulateMachine.run(unlockedMachine)

        assertThat(outputs, Matchers.`is`(listOf(NoAction)))
        assertThat(machine, Matchers.`is`(unlockedMachine))
    }

    @Test
    fun `testKnobOnUnLockedMachine`() {
        val unlockedMachine = myMachine.copy(locked = false)
        val simulateMachine = simulateMachine(listOf(Turn))
        val (outputs, machine) = simulateMachine.run(unlockedMachine)

        assertThat(outputs, Matchers.`is`(listOf(Candy)))
        assertThat(machine, Matchers.`is`(unlockedMachine.copy(locked = true, candies = unlockedMachine.candies - 1)))
    }

    @Test
    fun `testKnobOnEmptyMachine`() {
        val unlockedEmptyMachine = myMachine.copy(locked = false, candies = 0)
        val simulateMachine = simulateMachine(listOf(Turn))
        val (outputs, machine) = simulateMachine.run(unlockedEmptyMachine)

        assertThat(outputs, Matchers.`is`(listOf(NoAction)))
        assertThat(machine, Matchers.`is`(unlockedEmptyMachine))
    }


    @Test
    fun `testMulitpleActions`() {

        val simulateMachine = simulateMachine2(listOf(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
        val (outputs, machine) = simulateMachine.run(myMachine)

        println(outputs)
        println(machine)
//        assertThat(outputs, Matchers.`is`(listOf(NoAction, Candy, NoAction, Candy, NoAction, Candy, NoAction, Candy)))
//        assertThat(machine, Matchers.`is`(myMachine.copy(locked = true, candies = 1, coins = 14)))
    }


}