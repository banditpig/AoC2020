package day3

import utils.*
typealias TreeMap = List<String>
typealias MapCell = Char
const val tree: MapCell = '#'

typealias Point = Pair<Int, Int>

fun itemAt( xy: Point, tm: TreeMap): Char {
    if (xy.second > tm.size) {
        return '.'
    }
    return    tm[xy.second ][xy.first % 31]
}

fun allPoints(size: Int, stepF: (Point) -> Point): List<Point> =

     (0 until size).fold(mutableListOf(Point(0,0))){ acc: MutableList<Pair<Int, Int>>, _ ->
         val prev = acc.last()
         val next = stepF(prev)
         acc.add(next)
         acc}

fun part1(treeMap: List<String>, stepFunc: (Point) -> Point): Int{
    val all = allPoints(treeMap.size - 1) { stepFunc(it) }
    return all.map { p -> itemAt(p, treeMap)}
            .filter { it == tree }
            .count()
}
fun part2(treeMap: List<String>): Long{
    val f1 = { xy: Point -> Point(xy.first + 1, xy.second + 1 )}
    val f2 = { xy: Point -> Point(xy.first + 3, xy.second + 1 )}
    val f3 = { xy: Point -> Point(xy.first + 5, xy.second + 1)}
    val f4 = { xy: Point -> Point(xy.first + 7, xy.second + 1 )}
    val f5 = { xy: Point -> Point(xy.first + 1, xy.second + 2)}
    val allFs = listOf(f1, f2, f3, f4, f5)

    return allFs.map {part1(treeMap, it)}
            .fold(1, {acc, x -> x * acc })
}

fun main(args: Array<String>) {
    val treeMap = readFileAsLines("resources/Day3Part1.txt")
    val f = { xy: Point -> Point(xy.first + 3, xy.second + 1 )}
    println(part1(treeMap, f))
    print(part2(treeMap))
}