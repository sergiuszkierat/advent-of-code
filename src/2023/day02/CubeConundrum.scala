package day02

@main
def cubeConundrum(): Unit =
  val lines = os.read.lines(os.pwd / os.up / "src" / "2023" / "day02" / "input.txt")
  val result1 = part1(lines)
  println(s"Part1 : $result1")
  val result2 = part2(lines)
  println(s"Part2 : $result2")

type GameNo = Int

case class Cube(howMany: Int, color: Color)

case class Game(number: GameNo, cubes: Seq[Seq[Cube]])

enum Color(val max: Int):
  case Red   extends Color(12)
  case Green   extends Color(13)
  case Blue   extends Color(14)

def part1(lines: Seq[String]) =
  lines.flatMap { line =>
    val game = parseGame(line)
    val possible = game.cubes.map(isPossible).forall(_ == true)
    Option.when(possible)(game.number)
  }.sum

def part2(lines: Seq[String]) =
  lines.map { line =>
    val game = parseGame(line)
    val fewest = fewestNumberOfCubes(game.cubes)
    fewest.map(_.howMany).product
  }.sum

def parseGame(line: String): Game =
  val (gameNo, gameSets) = line match {
    case s"Game $number: $sets" => (number.toInt, sets)
  }
  Game(gameNo, gameSets.split("; ").map(cubes))

def cubes(setOfCubes: String): Seq[Cube] =
  setOfCubes.split(", ").map {
    case s"$number $color" => Cube(number.toInt, Color.valueOf(color.capitalize))
  }

def isPossible(cubes: Seq[Cube]): Boolean =
  cubes.forall(cube => cube.howMany <= cube.color.max)

def fewestNumberOfCubes(cubes: Seq[Seq[Cube]]): Seq[Cube] =
  cubes.flatten.groupBy(_.color).map {
    case (_, cubes) => cubes.maxBy(_.howMany)
  }.toSeq