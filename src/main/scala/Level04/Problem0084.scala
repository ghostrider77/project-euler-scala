package Level04

object Problem0084 {
  import scala.util.Random

  private val rng: Random = Random(2112)
  private val BoardSize: Int = 40

  case class Dice(sides: Int) extends AnyVal
  case class GameState(position: Int, ccCard: Int, chanceCard: Int, consecutiveDoubles: Int, visited: Map[Int, Int])

  private def rollTheDice(dice: Dice): Int =
    rng.between(1, dice.sides + 1)

  private def checkDouble(counter: Int, d1: Int, d2: Int): Int =
    if d1 == d2 then math.min(3, counter) else 0

  private def findNext(ps: List[Int], position: Int): Int =
    ps.find(_ > position) match
      case None => ps.head
      case Some(p) => p

  private def modifyPositionAtCommunityChest(position: Int, card: Int): Int =
    if card == 1 then 0
    else if card == 2 then 10
    else position

  private def modifyPositionAtChance(position: Int, card: Int): Int =
    if card == 1 then 0
    else if card == 2 then 10
    else if card == 3 then 11
    else if card == 4 then 24
    else if card == 5 then 39
    else if card == 6 then 5
    else if card == 7 || card == 8 then findNext(List(5, 15, 25, 35), position)
    else if card == 9 then findNext(List(12, 28), position)
    else if card == 10 then (position + BoardSize - 3) % BoardSize
    else position

  private def adjustPosition(position: Int, counter: Int, ccCard: Int, chanceCard: Int): (Int, Int, Int) =
    if position == 30 || counter == 3 then (10, ccCard, chanceCard)
    else if Set(2, 17, 33).contains(position) then
      (modifyPositionAtCommunityChest(position, ccCard), ccCard % 16 + 1, chanceCard)
    else if Set(7, 22, 36).contains(position) then
      (modifyPositionAtChance(position, chanceCard), ccCard, chanceCard % 16 + 1)
    else (position, ccCard, chanceCard)

  private def mostCommonPositions(counts: Map[Int, Int]): String =
    counts
      .toList
      .sortBy((_, count) => count)(Ordering[Int].reverse)
      .take(3)
      .map{ case (id, _) => f"$id%02d" }
      .mkString

  def playMonopoly(diceSides: Int, n: Int): String =
    val dice = Dice(diceSides)

    def update(value: Option[Int]): Option[Int] = value match
      case None => Some(1)
      case Some(v) => Some(v + 1)

    def playRound(state: GameState): GameState =
      val d1: Int = rollTheDice(dice)
      val d2: Int = rollTheDice(dice)
      val counter: Int = checkDouble(state.consecutiveDoubles, d1, d2)
      val position: Int = (state.position + d1 + d2) % BoardSize
      val (nextPosition, nextCcCard, nextChanceCard): (Int, Int, Int) =
        adjustPosition(position, counter, state.ccCard, state.chanceCard)
      val updatedVisit: Map[Int, Int] = state.visited.updatedWith(nextPosition)(update)
      GameState(nextPosition, nextCcCard, nextChanceCard, counter, updatedVisit)

    val finalState: GameState = (0 until n).foldLeft(GameState(0, 1, 1, 0, Map()))((acc, _) => playRound(acc))
    mostCommonPositions(finalState.visited)

  def main(args: Array[String]): Unit =
    val n: Int = 1000000
    val diceSides: Int = 4
    val result: String = playMonopoly(diceSides, n)
    println(result)
}
