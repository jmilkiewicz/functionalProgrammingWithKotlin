import arrow.core.None
import arrow.core.Option
import arrow.core.Some

data class Player(val name: String, val score: Int)

fun winner(p1: Player, p2: Player): Option<Player> =
    when {
        p1.score > p2.score -> Some(p1)
        p1.score < p2.score -> Some(p2)
        else -> None
    }

fun winnerMsg(op: Option<Player>): String =
    when (op) {
        is Some -> "${op.value.name} is the winner"
        is None -> "It's a draw"
    }

interface IO<A> {

    fun run(): A

    fun <B> map(f: (A) -> B): IO<B> =
        object : IO<B> {
            override fun run(): B = f(this@IO.run())
        }

    fun <B> flatMap(f: (A) -> IO<B>): IO<B> =
        object : IO<B> {
            override fun run(): B = f(this@IO.run()).run()
        }

    infix fun <B> assoc(io: IO<B>): IO<Pair<A, B>> =
        object : IO<Pair<A, B>> {
            override fun run(): Pair<A, B> =
                this@IO.run() to io.run()
        }

    companion object {

        fun <A> unit(a: () -> A) = object : IO<A> {
            override fun run(): A = a()
        }

        operator fun <A> invoke(a: () -> A) = unit(a)
    }
}

fun stdin(): IO<String> = IO { readLine().orEmpty() }

fun stdout(msg: String): IO<Unit> = IO { println(msg) }

fun contest(p1: Player, p2: Player): IO<Unit> =
    stdout(winnerMsg(winner(p1, p2)))


fun fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

fun converter(): IO<Unit> =
    stdout("Enter a temperature in degrees Fahrenheit: ").flatMap {
        stdin().map { it.toDouble() }.flatMap { df ->
            stdout("Degrees Celsius: ${fahrenheitToCelsius(df)}")
        }
    }


object IORun {

    val echo: IO<Unit> = stdin().flatMap(::stdout)

    val echo2: IO<Unit> = stdout("Napisz coś huju").flatMap { echo }

    val readInt: IO<Int> = stdin().map { it.toInt() }

    val readInts: IO<Pair<Int, Int>> = readInt assoc readInt

    fun factorial(x: Int) = (2..x).fold(1) { acc, e -> acc * e }

    val factorialIO: IO<Unit> = stdout(
        "The Amazing Factorial REPL, v0.1\n" +
                "q - quit\n" +
                "<number> - compute the factorial of the given number\n" +
                "<anything else> - crash spectacularly"
    ).flatMap {
        loop()
    }



    private fun loop(): IO<Unit> = stdin().flatMap { input ->
        when (input) {
            "q" -> stdout("bye")
            else -> {
                val toInt = input.toInt()
                stdout("factorial ${factorial(toInt)}").flatMap { loop() }
            }
        }
    }

    @JvmStatic
    fun main(args: Array<String>) {
        factorialIO.run()
    }
}