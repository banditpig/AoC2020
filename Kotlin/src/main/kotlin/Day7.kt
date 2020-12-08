package day7


import java.io.File


fun parseToMap(allData: String): MutableMap<String, List<Pair<Int, String>>>{
    fun twoHalves(l: String):Pair<String, String>{
        val split = l.split(" contain ")
        return Pair(split[0].trim(), split[1].trim())
    }
    fun bags(s: String):List<Pair<Int,String>>{
        if (s.contains("no other bags")) {
            return emptyList()
        }
        
        val bagStr = s.split(",")
        return bagStr.fold(mutableListOf<Pair<Int, String>>()){acc, b ->
            val match = Regex("(^\\d+)(\\D+)").find(b.trim())!!
            var (num,   desc ) = match.destructured
            if (num.toInt() == 1){
                desc += "s"
                acc.add(Pair(num.toInt(), desc.trim()))
            }else{
                acc.add(Pair(num.toInt(), desc.trim()))
            }

            acc
        }

    }
    val pairs = allData.lines().map{l -> twoHalves(l)}

    val ma = mutableMapOf<String, List<Pair<Int,String> >>()
    return  pairs.fold(ma){ acc, pair -> acc[pair.first] = bags(pair.second); acc}

}

fun countBagContent(n: String, map: MutableMap<String, List<Pair<Int, String>>>): Int =
     map[n]!!.map { pair -> pair.first*(1 + countBagContent(pair.second,map)) }
             .sum()

fun bagColours(map: MutableMap<String, List<Pair<Int, String>>>): Int  {

    fun findOwnerBagFor(target: String, hit: MutableSet<String>) {
        map.keys.map() { k -> map[k]!!.filter { v -> v.second.contains(target) }.forEach() { _ ->
            hit.add(k);
            findOwnerBagFor(k, hit) }}
    }
    val hitList = mutableSetOf<String>()
    findOwnerBagFor("shiny gold bag", hitList)
    return hitList.size
}


fun main(args: Array<String>) {
    val data = File("resources/Day7Part1.txt").readText()
    val all = parseToMap(data)
    println("Part 1.")
    println( bagColours(all))
    println("Part 2.")
    println( countBagContent("shiny gold bags",all))

}