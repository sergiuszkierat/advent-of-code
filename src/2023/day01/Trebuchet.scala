package day02

import scala.util.matching.Regex

@main
def trebuchet(): Int =
  val lines = os.read.lines(os.pwd / os.up / "src" / "2023" / "day01" / "input.txt")
  val result1 = part1(lines)
  println(s"Part1 : $result1")
  val result2 = part2(lines)
  println(s"Part2 : $result2")
  result2

val firstNumber1: Regex = "([1-9])".r
val firstNumber2: Regex = "([1-9]|one|two|three|four|five|six|seven|eight|nine)".r
val lastNumber1: Regex = "([1-9])".r
val lastNumber2: Regex = "([1-9]|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin)".r

def part1(lines: Seq[String]): Int = lines.map(calibration(_, firstNumber1, lastNumber1)).sum
def part2(lines: Seq[String]): Int = lines.map(calibration(_, firstNumber2, lastNumber2)).sum

def calibration(line: String, firstPattern: Regex, lastPattern: Regex): Int =
  val firstNumber = firstPattern.findFirstMatchIn(line)
  val lastNumber = lastPattern.findFirstMatchIn(line.reverse)
  val first = firstNumber.map(_.group(0)).map(number).getOrElse(0)
  val last = lastNumber.map(_.group(0)).map(number).getOrElse(0)
  s"$first$last".toInt

def number(v: String): Int = 
  v match {
    case "one" | "eno" | "1"  => 1
    case "two" | "owt" | "2" => 2
    case "three" | "eerht" | "3" => 3
    case "four" | "ruof" | "4" => 4
    case "five" | "evif" | "5" => 5
    case "six" | "xis" | "6" => 6
    case "seven" | "neves" | "7" => 7
    case "eight" | "thgie" | "8" => 8
    case "nine" |"enin" | "9" => 9
  }