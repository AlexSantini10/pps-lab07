package ex3


object Solitaire extends App:
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def arrayCoord(width: Int)(x: Int, y: Int): Int =
    y * width + x

  def nextPositions(width: Int, height: Int)(x: Int, y: Int): Seq[(Int, Int)] =
    Seq(
      (x + 3, y),
      (x - 3, y),
      (x, y + 3),
      (x, y - 3),
      (x + 2, y + 2),
      (x + 2, y - 2),
      (x - 2, y + 2),
      (x - 2, y - 2)
    ).filter((nx, ny) =>
      nx >= 0 && nx < width &&
        ny >= 0 && ny < height
    )


  def placeMarks(start: (Int, Int), width: Int, height: Int): List[List[(Int, Int)]] =
    var currNum = 1
    val totalCells = width * height
    val board = Array.fill(totalCells)(0)
    var solutions: List[List[(Int, Int)]] = List()

    val mapCoord = arrayCoord(width)
    val mapNextPositions = nextPositions(width, height)

    var currSol = List[(Int, Int)]()

    def search(x: Int, y: Int): Unit =
      board(mapCoord(x, y)) = currNum
      currSol = (x, y) :: currSol

      if currNum == totalCells then
        solutions = currSol :: solutions
      else
        currNum += 1

        mapNextPositions(x, y)
          .filter((nx, ny) => board(mapCoord(nx, ny)) == 0)
          .foreach(search)

        currNum -= 1

      board(mapCoord(x, y)) = 0
      currSol = currSol.tail

    search(start._1, start._2)

    solutions

  def placeMarks(width: Int, height: Int): List[List[(Int, Int)]] =
    val start = (width / 2, height / 2)
    placeMarks(start, width, height)

  val startTime = System.currentTimeMillis()
  val width = 5
  val height = 7
  val start = (width / 2, height / 2)
  println(s"Solitario del 35 on $width x $height, start at center $start ...")

  val sols = placeMarks(width, height)
  println(s"Found ${sols.size} solutions")
  sols.headOption.foreach(s => println(render(s, width, height)))

  val endTime = System.currentTimeMillis()
  println(s"Execution time: ${endTime - startTime} ms")
