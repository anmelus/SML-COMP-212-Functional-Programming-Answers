fun intToString(x : int) : string = Int.toString x

val y : string = "th"

fun double(n : int) : int = 2 * n

fun suffix(x : int) = intToString(x) ^ y 

val z : string = suffix 4