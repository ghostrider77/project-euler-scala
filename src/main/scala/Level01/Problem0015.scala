package Level01

object Problem0015 {
  def nChooseK(n: Int, k: Int): Long =
    if k == 0 then 1L
    else nChooseK(n - 1, k - 1) * n / k

  def main(args: Array[String]): Unit =
    val gridSize: Int = 20
    val result: Long = nChooseK(2 * gridSize, gridSize)
    println(result)
}
