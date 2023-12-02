import scala.util.matching.Regex

@main
def trebuchet(): Int =
  val lines = os.read.lines(os.pwd / os.up / "src" / "2023" / "day01" / "input.txt")
  val result = part1(lines)
  println(s"Part1 : $result")
  result

def part1(lines: Seq[String]): Int = lines.map(calibration).sum

val firstNumberPattern: Regex = "([1-9]|one|two|three|four|five|six|seven|eight|nine)".r
val lastNumberPattern: Regex = "([1-9]|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin)".r

def calibration(line: String): Int =
  val firstNumber = firstNumberPattern.findFirstMatchIn(line)
  val lastNumber = lastNumberPattern.findFirstMatchIn(line.reverse)
  val first = firstNumber.map(_.group(0)).flatMap(numbers.get).getOrElse(0)
  val last = lastNumber.map(_.group(0)).flatMap(numbers.get).getOrElse(0)
  println(s"$line: $first$last")
  s"$first$last".toInt

val numbers = Map(
  "one" -> 1,
  "eno" -> 1,
  "two" -> 2,
  "owt" -> 2,
  "three" -> 3,
  "eerht" -> 3,
  "four" -> 4,
  "ruof" -> 4,
  "five" -> 5,
  "evif" -> 5,
  "six" -> 6,
  "xis" -> 6,
  "seven" -> 7,
  "neves" -> 7,
  "eight" -> 8,
  "thgie" -> 8,
  "nine" -> 9,
  "enin" -> 9,
  "1" -> 1,
  "2" -> 2,
  "3" -> 3,
  "4" -> 4,
  "5" -> 5,
  "6" -> 6,
  "7" -> 7,
  "8" -> 8,
  "9" -> 9
)