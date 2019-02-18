package example
import spray.json._
import DefaultJsonProtocol._
object Main extends App {

  def printList(args: TraversableOnce[_]): Unit = {
    args.foreach(println)
  }

  //"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}"
  type Space = List[List[Int]]
  //\"players\":[[[3,5],[2,5]],[[3,4],[4,4]]]
  type Players = List[List[List[Int]]]
  case class Board(turn: Int, players: Players, spaces: Space)
  implicit val boardFormat = jsonFormat3(Board)

  val directions = List(-1, 0, 1).flatMap(x => List(-1, 0, 1).map(y => List(x, y))).filterNot(_ == List(0, 0))

  def availableMove(b: Board): List[List[List[Int]]] = {
    val mytokens = b.players(0)
    val space = b.spaces
    mytokens.map {
      t => {
        val surroundings = directions.map(
          d => t.zip(d).map(
            x => x._1 + x._2))
        surroundings.filterNot {
          l => {
            val r = l(0) - 1
            val c = l(1) - 1
            lazy val notOnBoard = r < 0 || r >= 5 || c < 0 || c >= 5
            lazy val isTokenPositioned = b.players.exists(_.contains(l))
            lazy val isTowerTooHigh = (space(r)(c) - space(t(0) - 1)(t(1) - 1)) > 1
            lazy val isTowerBuilt = space(r)(c) == 4
            (notOnBoard || isTokenPositioned || isTowerTooHigh || isTowerBuilt)
          }
        }
      }
    }

  }

  //todo : minimax strategy
  def evaluateBoard(b: Board): Int = {
    //todo add evaluation
    1
  }

  def updatePlayer(p: Players, c: Int, v: List[Int]): Players = {
    p
      .updated(0, p(0)
        .updated(c, v))
  }

  def changePlayer(p: Players): Players = {
    List(p(1),p(0))
  }

  def updateSpace(b: Space, r: Int, c: Int, v: Int): Space = {
    b
      .updated(r, b(r)
        .updated(c, v))
  }

  def makeStep(b: Board, nextStep: (move,build), token_idx : Int): Board = {
    val turn = b.turn + 1

    val move = nextStep._1
    val player = changePlayer(updatePlayer(b.players, token_idx, move))

    val build = nextStep._2
    val r = build(0)-1
    val c = build(1)-1
    val space = updateSpace(b.spaces, r , c, b.spaces(r)(c) + 1)

    Board(turn,player,space)
  }

  type move = List[Int] //len 2 tuple
  type build = List[Int] //len 2 tuple

  // dimensions:
  // moves  :: token / moves
  def availableBuild(b: Board, moves: List[List[move]]): List[List[(move,build)]] = {
    val space = b.spaces

    val token_builds = moves.zipWithIndex.map{
      zipped => {
        val token = zipped._1
        val idx = zipped._2
        token.map {
          t => {
            val surroundings = directions.map(
              d => t.zip(d).map(
                x => x._1 + x._2))
            val player = updatePlayer(b.players, idx, t)

            surroundings.filterNot {
              l => {
                val r = l(0) - 1
                val c = l(1) - 1
                lazy val notOnBoard = r < 0 || r >= 5 || c < 0 || c >= 5
                lazy val isTokenPositioned = player.exists(_.contains(l))
                lazy val isTowerBuilt = space(r)(c) == 4
                (notOnBoard || isTokenPositioned || isTowerBuilt)
              }
            }
          }
        }
      }
    }

    val ret = token_builds.zip(moves).map{
      t => {
        val builds = t._1
        val moves_ = t._2
        builds.zip(moves_).flatMap {
          zipped => {
            val move = zipped._2
            val build = zipped._1
            build.map(b => (move, b))
          }
        }
      }
    }
    ret
  }


  def main(): Unit ={

    //val testjson = " {\"turn\":19, \"players\":[[[3,5],[2,5]],[[3,4],[4,4]]],  \"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}"
    //val testjson = " {\"turn\":0, \"players\":[],  \"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]]}"
    //val testjson = " {\"turn\":0, \"players\":[[[3,3],[3,4]]],  \"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]]}"
//  {"turn":19, "players":[[[3,5],[2,5]],[[3,4],[4,4]]],  "spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}
    val testjson = Iterator.
      continually(scala.io.StdIn.readLine).
      takeWhile(_ != null).
      mkString("\n")
    //board
    val gameBoard = testjson.parseJson.convertTo[Board]
    //List(List(-1, -1), List(-1, 0), List(-1, 1), List(0, -1), List(0, 1), List(1, -1), List(1, 0), List(1, 1))

    if (gameBoard.players.isEmpty){
      val turn = 0

      val player = List(List(List(3,3),List(4,4)))

      val new_board = Board(turn,player,gameBoard.spaces)
      println(new_board.toJson)

    }else if(gameBoard.players.length == 1){
      val turn = gameBoard.turn + 1
      if (gameBoard.players(0).contains(List(3,3))){
        if(gameBoard.players(0).contains(List(4,4))){
          val player = List(List(List(3,4),List(3,2)),gameBoard.players(0))
          val new_board = Board(turn,player,gameBoard.spaces)
          println(new_board.toJson)
        }else{
          val player = List(List(List(3,4),List(4,4)),gameBoard.players(0))
          val new_board = Board(turn,player,gameBoard.spaces)
          println(new_board.toJson)
        }
      }else{
        if(gameBoard.players(0).contains(List(4,4))){
          val player = List(List(List(3,3),List(3,2)),gameBoard.players(0))
          val new_board = Board(turn,player,gameBoard.spaces)
          println(new_board.toJson)
        }else{
          val player = List(List(List(3,3),List(4,4)),gameBoard.players(0))
          val new_board = Board(turn,player,gameBoard.spaces)
          println(new_board.toJson)
        }
      }
    } else{
      val potentialMoves = availableMove(gameBoard)
      // move build pairs
      val steps = availableBuild(gameBoard,potentialMoves)

      val new_board =  if (steps(0).isEmpty)
        makeStep(gameBoard,steps(1)(0),1)
      else
        makeStep(gameBoard,steps(0)(0),0)

      //println(testjson)
      println(new_board.toJson)
    }

  }

  main()

}