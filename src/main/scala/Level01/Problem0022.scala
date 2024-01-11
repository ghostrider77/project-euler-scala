package Level01

object Problem0022 {
  def calcNameScores(names: List[String]): Int =
    val alphabet: List[Char] = ('A' to 'Z').toList
    def nameValue(name: String): Int = name.foldLeft(0)((acc, char) => acc + alphabet.indexOf(char) + 1)
    names.sorted.zipWithIndex.foldLeft(0){ case (acc, (name, ix)) => acc + (ix + 1) * nameValue(name) }

  def main(args: Array[String]): Unit =
    val line: String = scala.io.Source.fromResource("input0022.txt").getLines().next()
    val names: List[String] = line.split(",").toList
    val result: Int = calcNameScores(names)
    println(result)
}
