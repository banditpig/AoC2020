package day2
import utils.*

data class Criteria(private val criteriaPasswd: String){
    val minMax : Pair<Int,Int>
    val letter: Char
    val password: String
   init{
       //7-9 t: ttzmqqdtqtt
       val (criteria, pwd) = Pair(
               criteriaPasswd.split(":")[0],
               criteriaPasswd.split(":")[1])
       this.password = pwd

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
fun validate(cr: Criteria): Boolean{
    val letterCount = cr.password.filter {it == cr.letter }
                                 .count()
    return  letterCount >= cr.minMax.first
            &&
            letterCount <= cr.minMax.second

}



fun part1(): Int{
    return readFileAsLines("resources/Day2Part1.txt")
            .map { Criteria(it) }
            .filter{validate(it)}
            .count()
}

fun main(args: Array<String>) {
   println(part1())

}