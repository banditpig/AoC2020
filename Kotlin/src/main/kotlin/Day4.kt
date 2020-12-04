package day4
import java.io.File

typealias Key = String
typealias Value = String
typealias Passport = List<Pair<Key, Value>>

val  keysNoCid = listOf("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

val validators = mapOf(
    "byr" to  ::birthCheck,
    "iyr" to  ::issueCheck,
    "eyr" to  ::expireCheck,
    "hgt" to  ::heightCheck,
    "hcl" to  ::hairCheck,
    "ecl" to  ::eyeCheck,
    "pid" to  ::pidCheck,
    "cid" to  { true}
)

fun numberCheck(lo:Int, hi: Int, num: Int ): Boolean = num in lo..hi
fun birthCheck(yr: String): Boolean =
        numberCheck(1920, 2002, yr.toInt())
fun issueCheck(yr: String): Boolean =
        numberCheck(2010, 2020, yr.toInt())
fun expireCheck(yr: String): Boolean =
        numberCheck(2020, 2030, yr.toInt())

fun hairCheck(hair: String): Boolean =
        hair.matches(Regex("#[0-9,a-f]{6}"))

fun eyeCheck(eye: String): Boolean =
        listOf("amb","blu","brn", "gry", "grn", "hzl","oth").contains(eye)

fun pidCheck(pid: String): Boolean =
        pid.matches(Regex("^[0-9]{9}\$"))


fun heightCheck(height: String): Boolean{
    val post = height.takeLast(2)
    val numStr = height.dropLastWhile { it.isLetter() }
    return when(post){
        "cm" -> numberCheck(150, 193, numStr.toInt())
        "in" -> numberCheck(59, 76, numStr.toInt())
        else -> false
    }
}



fun parseLine(line: String):  List<Pair<Key, Value>> =
    line.split(' ')
        .map { Pair(it.split(':')[0], it.split(':')[1]) }

fun parseBlock(block: String): Passport =
    block.lines().flatMap { l -> parseLine(l) }

fun parseAll(all: String): List<Passport> =
    all.split("\r\n\r\n").map { parseBlock(it) }

fun passportHasValidKeys(p: Passport): Boolean{
  val keyList= p.fold(mutableListOf<String>(), { acc, kv  ->
      val k = kv.first
      if (k != "cid") acc.add(kv.first)
      acc
    } )
    return keyList.containsAll(keysNoCid)
}
fun passportHasValidValues(p: Passport) : Boolean =
     p.map { kv ->
        validators.getValue(kv.first).invoke(kv.second)
    }.filter { it }.count() == p.size



fun passportsWithValidKeys(passports: List<Passport>):List<Passport> {
    return passports.fold(mutableListOf(), { acc, p ->
        if(passportHasValidKeys(p)) acc.add(p)
        acc
        })
}
fun part2(passports: List<Passport>): Int =
       passportsWithValidKeys(passports)
               .map { passportHasValidValues(it) }
               .filter { it }
               .size

fun main(args: Array<String>) {
    val data = File("resources/Day4Part1.txt").readText()
    val all = parseAll(data)
    println("Part 1")
    println(passportsWithValidKeys(all).size)

    println("Part 2")
    println(part2(all))

}
