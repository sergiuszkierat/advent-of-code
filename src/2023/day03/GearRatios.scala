package day03

@main
def gearRatios(): Unit =
  val lines   = os.read.lines(os.pwd / os.up / "src" / "2023" / "day03" / "input.txt")
  val result1 = part1(lines.take(2))
  println(s"Part1 : $result1")
  val result2 = part2(lines)
  println(s"Part2 : $result2")

case class PartCandidate(value: Int, start: Int, end: Int)
case class Symbol(value: String, idx: Int)
case class Line(lineIdx: Int, partCandidates: Seq[PartCandidate], symbols: Seq[Symbol])

def part1(lines: Seq[String]) =
  val schematic: Seq[Line] = lines.zipWithIndex.map { case (line, lineIdx) =>
    readLine(line, lineIdx)
  }
  schematic.map { case Line(lineIdx, partCandidates, symbols) =>
    partCandidates.map { case PartCandidate(value, start, end) =>
      1
      2
    }
  }

def part2(lines: Seq[String]) = 2

def readLine(line: String, lineIdx: Int): Line =
  val partCandidates = for (number <- """\d+""".r findAllMatchIn line)
    yield PartCandidate(number.group(0).toInt, number.start, number.end)
  val symbols = for (symbol <- """[^.\d]""".r findAllMatchIn line)
    yield Symbol(symbol.group(0), symbol.start)
  Line(lineIdx, partCandidates.toSeq, symbols.toSeq)
