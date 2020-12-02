package day2
import utils.*

data class CriteriaPwd(private val criteriaPasswd: String){
    val minMax : Pair<Int,Int>
    val letter: Char
    val password: String
   init{
       //7-9 t: ttzmqqdtqtt
       val (criteria, pwd) = Pair(
               criteriaPasswd.split(":")[0],
               criteriaPasswd.split(":")[1])
       this.password = pwd.trim()

       //14-15 d
       val halves = criteria.split(' ')
       val (range, letter) = Pair(halves[0], halves[1])
       this.letter = letter[0]

       this.minMax =
                  Pair(range.split("-")[0]
                               .toInt(),
                       range.split("-")[1]
                               .toInt())
   }
}
fun validate1(cr: CriteriaPwd): Boolean{
    val letterCount = cr.password
            .filter {it == cr.letter }
            .count()

    return  letterCount >= cr.minMax.first
            &&
            letterCount <= cr.minMax.second

}
fun validate2(cr: CriteriaPwd): Boolean =

    listOf(
            cr.password[cr.minMax.first - 1],
            cr.password[cr.minMax.second - 1],
    )
            .filter { it == cr.letter }
            .count() == 1


fun part1(): Int{

    return readFileAsLines("resources/Day2Part1.txt")
            .map { CriteriaPwd(it) }
            .filter{validate1(it)}
            .count()
}
fun part2(): Int{

    return readFileAsLines("resources/Day2Part1.txt")
            .map { CriteriaPwd(it) }
            .filter{validate2(it)}
            .count()
}

fun main(args: Array<String>) {
    println(part1())
    println(part2())

}