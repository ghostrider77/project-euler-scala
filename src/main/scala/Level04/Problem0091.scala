package Level04

import math.Ordering.Implicits.infixOrderingOps

object Problem0091 {
  private case class Point(x: Int, y: Int)

  private object Point:
    given Ordering[Point] with
      def compare(p: Point, q: Point): Int =
        if p.x > q.x || (p.x == q.x && p.y > q.y) then 1
        else if p.x == q.x && p.y == q.y then 0
        else -1

  private def distance(p: Point, q: Point): Double =
    val dx: Int = p.x - q.x
    val dy: Int = p.y - q.y
    math.hypot(dx, dy)

  def calcNumberOfRightTriangles(limit: Int): Int =
    val origin = Point(0, 0)
    val gridPoints: List[Point] = (for {
      a <- 0 to limit
      b <- 0 to limit
      if a != 0 || b != 0
    } yield Point(a, b)).toList
    val vertices: List[(Point, Point)] = for {
      p <- gridPoints
      q <- gridPoints
      if p < q
    } yield (p, q)

    def isRightTriangle(p: Point, q: Point): Boolean =
      val d1: Double = distance(p, origin)
      val d2: Double = distance(q, origin)
      val d3: Double = distance(p, q)
      List(d1, d2, d3).sorted match
        case List(a, b, c) =>
          val diff: Double = c * c - (a * a + b * b)
          math.abs(diff) < 1e-8
        case _ => throw Exception("The distance list should have 3 elements.")

    vertices.count(isRightTriangle)

  def main(args: Array[String]): Unit =
    val limit: Int = 50
    val result: Int = calcNumberOfRightTriangles(limit)
    println(result)
}
