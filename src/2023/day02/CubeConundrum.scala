package day02

@main
def cubeConundrum(): Unit =
  val lines = os.read.lines(os.pwd / os.up / "src" / "2023" / "day02" / "input.txt")
  val result1 = part1(lines)
  println(s"Part1 : $result1")
//  val result2 = part2(lines)
//  println(s"Part2 : $result2")
//  result2

type GameNo = Int

case class Cube(howMany: Int, color: Color)

enum Color(val max: Int):
  case Red   extends Color(12)
  case Green   extends Color(13)
  case Blue   extends Color(14)

def part1(lines: Seq[String]) = lines.flatMap(game).sum
//def part2(lines: Seq[String]): Int = lines.map(calibration(_, firstNumber2, lastNumber2)).sum

def game(line: String): Option[GameNo] =
  val (gameNo, gameSets) = line match {
    case s"Game $number: $sets" => (number.toInt, sets)
  }
  val possible = gameSets.split("; ").map(cubes).map(isPossible).forall(_ == true)
  Option.when(possible)(gameNo)

def cubes(setOfCubes: String): Seq[Cube] =
  setOfCubes.split(", ").map {
    case s"$number $color" => Cube(number.toInt, Color.valueOf(color.capitalize))
  }

def isPossible(cubes: Seq[Cube]): Boolean =
  cubes.forall(cube => cube.howMany <= cube.color.max)