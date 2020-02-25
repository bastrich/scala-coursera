package reductions

import org.scalameter._

object LineOfSightRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

sealed abstract class Tree {
  def maxPrevious: Float
}

case class Node(left: Tree, right: Tree) extends Tree {
  val maxPrevious = left.maxPrevious.max(right.maxPrevious)

  override def toString: String = {
    s"Node(left: ${left.toString}, right: ${right.toString})"
  }
}

case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree {
  override def toString: String = {
    s"Leaf(from: $from, until: $until, maxPrevious: $maxPrevious)"
  }
}

object LineOfSight extends LineOfSightInterface {

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    output(0) = 0
    var max = 0F
    for (i <- 1 until input.length) {
      max = Math.max(input(i) / i, max)
      output(i) = max
    }
  }

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    var max = 0F
    for (i <- from until until) {
      max = Math.max(input(i) / i, max)
    }
    max
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   * returns the reduction tree for that part of the array.
   *
   * The reduction tree is a `Leaf` if the length of the specified part of the
   * array is smaller or equal to `threshold`, and a `Node` otherwise.
   * If the specified part of the array is longer than `threshold`, then the
   * work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
              threshold: Int): Tree = {
    if (end - from <= threshold) {
      Leaf(from, end, upsweepSequential(input, from, end))
    } else {
      val res = parallel(upsweep(input, from, from + (end - from) / 2, threshold), upsweep(input, from + (end - from) / 2, end, threshold))
      Node(res._1, res._2)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   * `until`, and computes the maximum angle for each entry of the output array,
   * given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
                          startingAngle: Float, from: Int, until: Int): Unit = {
    var max = startingAngle
    for (i <- from until until) {
      max = Math.max(input(i) / i, max)
      output(i) = max
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   * reduction `tree` in parallel, and then calls `downsweepSequential` to write
   * the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
                tree: Tree): Unit = {

//    if (input.sameElements(Array(0.0f, 7.0f, 10.0f, 33.0f, 48.0f)) &&
//    Thread.currentThread().getStackTrace.exists(it => it.getMethodName.contains("non$minuszero")) ) {
//      throw new Exception(s"$startingAngle \n ${tree.toString}")
//    }

    tree match {
      case Node(left, right) => {
        parallel(downsweep(input, output, startingAngle, left), downsweep(input, output, math.max(startingAngle, left.maxPrevious), right))
      }
      case Leaf(from, until, maxPrevious) => downsweepSequential(input, output, startingAngle, from, until)
    }

  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
                     threshold: Int): Unit = {
    output(0) = input(0)
    downsweep(input, output, input(0), upsweep(input, 1, input.length, threshold))
  }
}
