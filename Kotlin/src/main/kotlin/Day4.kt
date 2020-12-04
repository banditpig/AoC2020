package day4
import java.io.File

typealias Key = String
typealias Value = String
typealias Passport = List<Pair<Key, Value>>

val  keysNoCid = listOf("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

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
fun passportHasValidValues(p: Passport) : Boolean{
    return false;
}

fun passportsWithValidKeys(passports: List<Passport>):List<Passport> {
    return passports.fold(mutableListOf<Passport>(), {acc, p ->
        if(passportHasValidKeys(p)) acc.add(p)
        acc
        })
}

fun main(args: Array<String>) {
    val data = File("resources/Day4Part1.txt").readText()
    val all = parseAll(data)
    println("Part 1")
    println(passportsWithValidKeys(all).size)


}
