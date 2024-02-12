package Level03

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.infixOrderingOps

object Problem0054 {
  enum Suit:
    private case Clubs, Diamonds, Hearts, Spades

  private object Suit:
    def apply(c: Char): Suit = c match
      case 'C' => Clubs
      case 'D' => Diamonds
      case 'H' => Hearts
      case 'S' => Spades
      case _ => throw Exception(s"Unknown suite $c.")

  case class Card(value: Int, suit: Suit)

  private object Card:
    def apply(s: String): Card = s.toCharArray.toList match
      case List(v, suit) =>
        val value: Int = v match
          case 'A' => 14
          case 'K' => 13
          case 'Q' => 12
          case 'J' => 11
          case 'T' => 10
          case _ => v.asDigit
        Card(value, Suit(suit))
      case _ => throw Exception(s"Unknown card $s.")

  extension [T: Ordering](xs: List[T])
    private def compare(ys: List[T]): Int =
      @tailrec
      def loop(xs: List[T], ys: List[T]): Int = (xs, ys) match
        case (Nil, Nil) => 0
        case (_ :: _, Nil) => 1
        case (Nil, _ :: _) => -1
        case (x :: xss, y :: yss) => if x > y then 1 else if x < y then -1 else loop(xss, yss)

      loop(xs, ys)

  sealed trait Rank
  private case object RoyalFlush extends Rank
  private case class StraightFlush(highest: Int) extends Rank
  private case class FourOfAKind(value: Int, high: Int) extends Rank
  private case class FullHouse(three: Int, pair: Int) extends Rank
  private case class Flush(high: Int) extends Rank
  private case class Straight(highest: Int) extends Rank
  private case class ThreeOfAKind(value: Int) extends Rank
  private case class TwoPairs(largerPair: Int, smallerPair: Int, high: Int) extends Rank
  private case class OnePair(pair: Int, highCardValuesDesc: List[Int]) extends Rank
  private case class HighCard(highCardValuesDesc: List[Int]) extends Rank

  object Rank:
    given Ordering[Rank] with
      def compare(r1: Rank, r2: Rank): Int = (r1, r2) match
        case (RoyalFlush, RoyalFlush) => 0
        case (RoyalFlush, _) => 1
        case (StraightFlush(h1), StraightFlush(h2)) => h1.compare(h2)
        case (StraightFlush(_), RoyalFlush) => -1
        case (StraightFlush(_), _) => 1
        case (FourOfAKind(v1, _), FourOfAKind(v2, _)) => v1.compare(v2)
        case (FourOfAKind(_, _), RoyalFlush | StraightFlush(_)) => -1
        case (FourOfAKind(_, _), _) => 1
        case (FullHouse(t1, _), FullHouse(t2, _)) => t1.compare(t2)
        case (FullHouse(_, _), RoyalFlush | StraightFlush(_) | FourOfAKind(_, _)) => -1
        case (FullHouse(_, _), _) => 1
        case (Flush(h1), Flush(h2)) => h1.compare(h2)
        case (Flush(_), RoyalFlush | StraightFlush(_) | FourOfAKind(_, _) | FullHouse(_, _)) => -1
        case (Flush(_), _) => 1
        case (Straight(h1), Straight(h2)) => h1.compare(h2)
        case (Straight(_), RoyalFlush | StraightFlush(_) | FourOfAKind(_, _) | FullHouse(_, _) | Flush(_)) => -1
        case (Straight(_), _) => 1
        case (ThreeOfAKind(v1), ThreeOfAKind(v2)) => v1.compare(v2)
        case (ThreeOfAKind(_), TwoPairs(_, _, _) | OnePair(_, _) | HighCard(_)) => 1
        case (ThreeOfAKind(_), _) => -1
        case (TwoPairs(lp1, sp1, h1), TwoPairs(lp2, sp2, h2)) =>
          if lp1 > lp2 then 1
          else if lp1 < lp2 then -1
          else if sp1 > sp2 then 1
          else if sp1 < sp2 then -1
          else h1.compare(h2)
        case (TwoPairs(_, _, _), OnePair(_, _) | HighCard(_)) => 1
        case (TwoPairs(_, _, _), _) => -1
        case (OnePair(p1, rest1), OnePair(p2, rest2)) =>
          if p1 > p2 then 1
          else if p1 < p2 then -1
          else rest1.compare(rest2)
        case (OnePair(_, _), HighCard(_)) => 1
        case (OnePair(_, _), _) => -1
        case (HighCard(hs1), HighCard(hs2)) => hs1.compare(hs2)
        case (HighCard(_), _) => -1

  case class Hand(cards: List[Card]):
    val rank: Rank =
      val values: List[Int] = cards.map(_.value).sorted(Ordering[Int].reverse)
      val aceReplaced: List[Int] = values.map(v => if v == 14 then 1 else v).sorted(Ordering[Int].reverse)
      val suits: Set[Suit] = cards.map(_.suit).toSet
      val consecutiveDifferences: List[Int] = values.sliding(2).collect{ case List(a, b) => a - b }.toList
      val counts: List[(Int, Int)] =
        values.groupMapReduce(identity)(_ => 1)(_ + _).toList.sortBy{ (_, count) => count }(Ordering[Int].reverse)

      if suits.size == 1 then
        if values == List(14, 13, 12, 11, 10) then RoyalFlush
        else if consecutiveDifferences.forall(_ == 1) then StraightFlush(values.head)
        else if aceReplaced == List(5, 4, 3, 2, 1) then StraightFlush(5)
        else Flush(values.head)
      else if consecutiveDifferences.forall(_ == 1) then Straight(values.head)
      else if aceReplaced == List(5, 4, 3, 2, 1) then Straight(5)
      else counts match
        case List((v, 4), (h, 1)) => FourOfAKind(v, h)
        case List((three, 3), (two, 2)) => FullHouse(three, two)
        case (three, 3) :: _ => ThreeOfAKind(three)
        case List((p1, 2), (p2, 2), (h, 1)) => TwoPairs(p1 max p2, p1 min p2, h)
        case (p, 2) :: rest => OnePair(p, rest.map(_._1).sorted(Ordering[Int].reverse))
        case _ => HighCard(values)

  case class Game(player1: Hand, player2: Hand)

  def readGame(line: String): Game =
    line.split(" ").toList match
      case List(p1c1, p1c2, p1c3, p1c4, p1c5, p2c1, p2c2, p2c3, p2c4, p2c5) =>
        val h1: List[Card] = List(p1c1, p1c2, p1c3, p1c4, p1c5).map(Card(_))
        val h2: List[Card] = List(p2c1, p2c2, p2c3, p2c4, p2c5).map(Card(_))
        Game(Hand(h1), Hand(h2))
      case _ => throw Exception("Malformed input.")

  def nrGamesWon(games: Iterator[Game]): Int =
    def hasPlayer1Won(game: Game): Boolean =
      game.player1.rank > game.player2.rank

    games.count(hasPlayer1Won)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input0054.txt").getLines()
    val games: Iterator[Game] = lines.map(readGame)
    val result: Int = nrGamesWon(games)
    println(result)
}
