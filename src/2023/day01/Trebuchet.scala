

@main
def trebuchet() =
  val lines = os.read.lines(os.pwd / os.up / "src" / "2023" / "day01" / "small.txt")
  println(s"Calibration of line : ${lines.take(1).size}")


def calibration(line: String) = 0



//1abc2
//pqr3stu8vwx
//a1b2c3d4e5f
//treb7uchet