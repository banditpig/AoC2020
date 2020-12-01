import java.io.File

fun readFileAsLines(fileName: String): List<String> =
     File(fileName).useLines {it.toList() }

fun <T> pairs(arr: List<T>): Sequence<Pair<T, T>> =
    sequence {
    for(i in 0 until  - 1)
        for(j in 0 until arr.size - 1)
            if (i != j){
                yield(Pair(arr[i], arr[j]))
            }

}

fun <T> triples(arr: List<T>): Sequence<Triple<T, T, T>> =
    sequence {
        for (i in 0 until arr.size - 1)
            for (j in 0 until arr.size - 1)
                for (k in 0 until arr.size - 1)
                    if ((i != j) && (j != k)) {
                        yield(Triple(arr[i], arr[j], arr[k]))
                    }

    }
