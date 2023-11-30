package Level01

import scala.annotation.tailrec

object Problem0019 {
  private val monthDurations: List[Int] = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  enum Day:
    case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday

    def next(): Day = this match
      case Monday => Tuesday
      case Tuesday => Wednesday
      case Wednesday => Thursday
      case Thursday => Friday
      case Friday => Saturday
      case Saturday => Sunday
      case Sunday => Monday

  private def isLeapYear(year: Int): Boolean =
    year % 4 == 0 && year % 400 != 0

  private def calcNrSundaysInYear(year: Int, firstDay: Day): (Int, Day) =
    monthDurations.zipWithIndex.foldLeft((0, firstDay)){
      case ((sundays, day), (nrDays, index)) =>
        val daysInMonth: Int = if index == 1 && isLeapYear(year) then nrDays + 1 else nrDays
        val nrSundays: Int = if day == Day.Sunday then sundays + 1 else sundays
        val firstDayInNextMonth: Day = (0 until daysInMonth).foldLeft(day)((acc, _) => acc.next())
        (nrSundays, firstDayInNextMonth)
    }

  def countSundays(firstYear: Int, firstDay: Day, lastYear: Int): Int =
    @tailrec
    def loop(year: Int, firstDayInYear: Day, nrSundays: Int): Int =
      if year > lastYear then nrSundays
      else
        val (nrSundaysInThisYear, firstDayInNextYear): (Int, Day) = calcNrSundaysInYear(year, firstDayInYear)
        loop(year + 1, firstDayInNextYear, nrSundays + nrSundaysInThisYear)

    loop(firstYear, firstDay, 0)

  def main(args: Array[String]): Unit =
    val result: Int = countSundays(1901, Day.Tuesday, 2000)
    println(result)
}
