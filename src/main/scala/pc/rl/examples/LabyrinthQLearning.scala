package pc.rl.examples

object LabyrinthQLearning extends App:

  import pc.rl.model.QMatrix
  import pc.rl.model.QMatrix.Action.*
  import pc.rl.model.QMatrix.*

  private val endpoint = (9, 9)
  private val obstacles = List((2, 3), (4, 3), (8, 8))
  private val items = List((5,5))
  private val map = createLabyrinthMap(obstacles, wall(1,0,3,3))
  private val shortcut = List((5,1) -> (8,4))

  private def printMap(map: List[(Int, Int)]) =
    print("-"*60)
    println()
    for y <- 0 to 9 do
      print("|")
      for x <- 0 to 9 do
        (x, y) match
          case `endpoint` => print("  E  ")
          case _ if obstacles.contains((x, y)) => print("  0  ")
          case _ if map.contains((x, y)) => print("  W  ")
          case _ if shortcut.exists(_._1 == (x, y)) => print("  S  ")
          case _ if shortcut.exists(_._2 == (x, y)) => print("  S  ")
          case _ if items.contains((x, y)) => print("  I  ")
          case _ => print(" "*5)
        print("|")
      println()
      print("-" * 60)
      println()

  private case class wall(xStart: Int, yStart: Int, xSize: Int, ySize: Int)

  private def createLabyrinthMap(obstacle: List[(Int, Int)], walls: wall*): List[(Int, Int)] =
    walls
      .flatMap(
        w =>
          for x <- w.xStart until w.xStart + w.xSize
              y <- w.yStart until w.yStart + w.ySize
          yield (x, y))
      .toList ++ obstacle

  val rl: QMatrix.Facade = Facade(
    width = endpoint._1 + 1,
    height = endpoint._2 + 1,
    initial = (0, 0),
    terminal = {
      case p if p == endpoint => true
      case _ => false
    },
    reward = {
      case (p, _) if map.contains(p) => Double.NegativeInfinity
      case (p, _) if items.contains(p) => 10
      case _ => -1
    },
    jumps = {
      case (p, _) if shortcut.exists(_._1 == p) => shortcut.find(_._1 == p).get._2
    },
    gamma = 0.9,
    alpha = 0.5,
    epsilon = 0.3,
    v0 = 1
  )

  val q0 = rl.qFunction
  println(rl.show(q0.vFunction, "%4.1f"))
  val q1 = rl.makeLearningInstance().learn(20000, 100, q0)
  println(rl.show(q1.vFunction, "%4.1f"))
  println(rl.show(s => actionToString(q1.bestPolicy(s)), "%7s"))
  printMap(map)

