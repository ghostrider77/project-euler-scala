package Level02

object Problem0040 {
  def champernownesConstantProduct(indices: List[Int]): Int =
    val length: Int = indices.max
    val digits: Vector[Int] =
      Iterator
        .from(0)
        .flatMap(_.toString.toCharArray.map(_.asDigit))
        .take(length + 1)
        .toVector
    indices.map(digits).product

  def main(args: Array[String]): Unit =
    val indices: List[Int] = (0 to 6).map(math.pow(10, _).toInt).toList
    val result: Int = champernownesConstantProduct(indices)
    println(result)
}
