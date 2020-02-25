package streams

import org.junit._
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.assertFalse

import Bloxorz._

class BloxorzSuite {
  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
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


  @Test def `terrain function level 1 (10pts)`: Unit =
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }

  @Test def `find char level 1 (10pts)`: Unit =
    new Level1 {
      assertEquals(Pos(1, 1), startPos)
    }


  @Test def `optimal solution for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(Block(goal, goal), solve(solution))
    }


  @Test def `optimal solution length for level 1 (5pts)`: Unit =
    new Level1 {
      assertEquals(optsolution.length, solution.length)
    }

  @Test def `test terrainFunction`: Unit = {
    new Level1 {
      assertEquals(true, terrainFunction(vector)(Pos(0, 0)))
      assertEquals(false, terrainFunction(vector)(Pos(0, 3)))
    }
  }

  @Test def `test findChar`: Unit = {
    new Level1 {
      assertEquals(Pos(1, 1), findChar('S', vector))
      assertEquals(Pos(4, 7), findChar('T', vector))
    }
  }

  @Test def `test isStanding`: Unit = {
    new Level1 {
      assertTrue(Block(Pos(3, 5), Pos(3, 5)).isStanding)
      assertFalse(Block(Pos(3, 5), Pos(3, 6)).isStanding)
    }
  }

  @Test def `test isLegal`: Unit = {
    new Level1 {
      assertTrue(Block(Pos(1, 4), Pos(1, 5)).isLegal)
      assertFalse(Block(Pos(3, 0), Pos(3, 1)).isLegal)
    }
  }

  @Test def `test neighbors`: Unit = {
    new Level1 {
      assertEquals(
        List(
          (Block(Pos(5, 2), Pos(5, 3)), Left),
          (Block(Pos(5, 5), Pos(5, 6)), Right),
          (Block(Pos(3, 4), Pos(4, 4)), Up),
          (Block(Pos(6, 4), Pos(7, 4)), Down)
        ),
        Block(Pos(5, 4), Pos(5, 4)).neighbors
      )

      assertEquals(
        List(
          (Block(Pos(5, 3), Pos(5, 3)), Left),
          (Block(Pos(5, 6), Pos(5, 6)), Right),
          (Block(Pos(4, 4), Pos(4, 5)), Up),
          (Block(Pos(6, 4), Pos(6, 5)), Down)
        ),
        Block(Pos(5, 4), Pos(5, 5)).neighbors
      )

      assertEquals(
        List(
          (Block(Pos(4, 3), Pos(5, 3)), Left),
          (Block(Pos(4, 5), Pos(5, 5)), Right),
          (Block(Pos(3, 4), Pos(3, 4)), Up),
          (Block(Pos(6, 4), Pos(6, 4)), Down)
        ),
        Block(Pos(4, 4), Pos(5, 4)).neighbors
      )
    }
  }

  @Test def `test legalNeighbors`: Unit = {
    new Level1 {
      assertEquals(
        List(
          (Block(Pos(1, 3), Pos(1, 4)), Left),
          (Block(Pos(2, 5), Pos(3, 5)), Down)
        ),
        Block(Pos(1, 5), Pos(1, 5)).legalNeighbors
      )
    }
  }

  @Test def `test neighborsWithHistory`: Unit = {
    new Level1 {
      assertEquals(
        List(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ),
        neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up))
      )
    }
  }

  @Test def `test newNeighborsOnly`: Unit = {
    new Level1 {
      assertEquals(
        Set(
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).to(LazyList),
        newNeighborsOnly(
          Set(
            (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
            (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
          ).to(LazyList),
          Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
        )
      )
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
