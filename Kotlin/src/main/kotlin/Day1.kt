package day1
import utils.*

fun numbers(): List<Int> =
        readFileAsLines("resources/Day1Part1.txt")
            .map { it.toInt() }

fun part1():Pair<Int, Int> {

    val x =pairs<Int>(numbers())
          return  pairs<Int>(numbers())
            .filter { xy -> xy.first + xy.second == 2020 }
            .toList()[0]
}
fun part2():Triple<Int, Int, Int> =

    triples<Int>(numbers())
        .filter { xyz -> xyz.first + xyz.second + xyz.third == 2020 }
        .toList()[0]


fun main(args: Array<String>) {
    val (x,y) = part1()
    println(x * y)
    val (i, j, k) = part2()
    println(i * j * k)



}