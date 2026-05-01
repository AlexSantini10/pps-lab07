package ex4

import java.util.OptionalInt

// Optional!
object ConnectThree extends App:
  val bound = 3
  val height = 4
  val width = 4
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  extension (board: Board)
    def at(x: Int, y: Int): Option[Disk] =
      board.find(d => d.x == x && d.y == y)

    def insert(x: Int, y: Int, player: Player): Option[Board] =
      if board.exists(d => d.x == x && d.y == y) then None
      else Some(Disk(x, y, player) +: board)

    def isBoardFull: Boolean =
      (0 until width).forall(x => firstAvailableRow(board, x).isEmpty)

    def winner(): Option[Player] =
      def toCheckLines(x: Int, y: Int): Seq[Seq[(Int, Int)]] =
        val directions = Seq(
          (1, 0),
          (0, 1),
          (1, 1),
          (1, -1)
        )

        val lines =
          for
            (dx, dy) <- directions
            start <- -2 to 0
          yield
            (0 until 3).map(i => (x + (start + i) * dx, y + (start + i) * dy))

        lines.filter(_.forall((x, y) => x >= 0 && y >= 0 && x < width && y < height))

      def has3Consecutives(disk: Disk): Boolean =
        val positions = board
          .filter(_.player == disk.player)
          .map(d => (d.x, d.y))
          .toSet

        toCheckLines(disk.x, disk.y)
          .exists(line => line.forall(positions.contains))

      board.find(has3Consecutives).map(_.player)

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.at(x, y).map(_.player)

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    val occupied = board.filter(d => d.x == x).map(d => d.y)
    (0 until height).find(y => !occupied.contains(y))


  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 until width
      y <- firstAvailableRow(board, x)
      newBoard <- board.insert(x, y, player)
    yield newBoard

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] =
    if moves == 0 then
      LazyList(Seq(Seq.empty))
    else
      for
        game <- computeAnyGame(player.other, moves - 1)
        current = game.head
        newBoard <-
          if current.winner().isDefined || current.isBoardFull then
            Seq(current)
          else
            placeAnyDisk(current, player)
      yield newBoard +: game

  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  println("EX 4: ")
// Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 4).foreach { g =>
    printBoards(g)
    println()
  }
//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...

  println("EX 5: ")
  computeAnyGame(O, 8).foreach { g =>
    printBoards(g)
    println(s"winner: ${g.head.winner()}")
    println()
  }
