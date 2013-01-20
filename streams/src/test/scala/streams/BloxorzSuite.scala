package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(2,2)), "2,2")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("isStanding") {
    new Level1 {
      assert(!Block(Pos(0,0), Pos(1,1)).isStanding)
      assert(Block(Pos(0,0), Pos(0,1)).isStanding)
      //assert(Block(Pos(0, 1), Pos(0,0)).isStanding)
    }
  }
  
  test("isLegal") {
    new Level1 {
      assert(Block(Pos(0,0), Pos(0,1)).isLegal, "1")
      assert(!Block(Pos(3,0), Pos(4,0)).isLegal, "2")
      assert(!Block(Pos(2,0), Pos(3,0)).isLegal, "3")
      
      assert(!Block(Pos(0,2), Pos(0,3)).isLegal, "4")
      assert(Block(Pos(0,2), Pos(0,2)).isLegal, "5")
      assert(!Block(Pos(0,3), Pos(0,3)).isLegal, "6")
    }
  }
  
  test("startBlock") {
    new Level1 {
      assert(startBlock == Block(Pos(1, 1), Pos(1, 1)))
    }
  }
  
  test("isDone?") {
    new Level1 {
      assert(!done(Block(Pos(1, 1), Pos(1, 1))), "! done")
      assert(done(Block(Pos(4, 7), Pos(4, 7))), "done")
    }
  }
  
  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
