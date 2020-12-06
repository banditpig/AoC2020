package day6

import java.io.File

fun parseAll(all: String): List<String> =
        all.split("\n\n")
fun blockUnion(b: String): Int{

    val chs =  b.lines().joinToString(separator = "")
    val uniq = chs.fold(mutableSetOf<Char>()){acc, c -> acc.add(c); acc}
    return  uniq.size
}
fun blockIntersect(b: String): Int{

    val uniq: List<MutableSet<Char>> = b.lines().map {it.fold( mutableSetOf<Char>(),{ acc, c -> acc.add(c); acc} )}
    val finalSet = uniq.reduce { acc, it -> acc.apply { retainAll(it) } }
    return  finalSet.size
}


fun part1(blocks: List<String>): Int{

   return blocks.map(){it -> blockUnion(it) }.sum()
}

fun part2(blocks: List<String>): Int{
    return blocks.map(){it -> blockIntersect(it) }.sum()
}
fun main(args: Array<String>) {
    val data = File("resources/Day6Part1.txt").readText()
    val all = parseAll(data)

    println(part1(all))
    println(part2(all))

}