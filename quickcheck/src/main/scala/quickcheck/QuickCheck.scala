package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      e <- arbitrary[Int]
      h <- genHeap
    } yield insert(e, h),
    for {
      paramOrder <- oneOf(true, false)
      h <- genHeap
    } yield if (paramOrder) meld(h, empty) else meld(empty, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOfTwo") = forAll { (h: H) =>
    val e1 = arbInt.arbitrary.sample.get
    val e2 = arbInt.arbitrary.sample.get
    val min = Math.min(e1, e2)
    findMin(insert(e2, insert(e1, empty))) == min
  }

  property("insertMin") = forAll { (h: H) =>
    val e = arbInt.arbitrary.sample.get
    val heap = insert(e, empty)
    isEmpty(deleteMin(heap))
  }

  property("sortedSequence") = forAll { (h: H) =>
    def sortedSeq(h: H): List[Int] = if (isEmpty(h)) Nil else findMin(h) :: sortedSeq(deleteMin(h))

    val result = sortedSeq(h)

    def isSorted(list: List[Int]): Boolean = {
      if (list.isEmpty || list.tail.isEmpty) true
      else {
        list.head <= list.tail.head && isSorted(list.tail)
      }
    }

    isSorted(result)
  }

  property("min of meld") = forAll { h1: H =>
    val h2 = arbHeap.arbitrary.sample.get

    if (isEmpty(h1) && isEmpty(h2)) {
      isEmpty(meld(h1, h2))
    } else if (isEmpty(h1)) {
      findMin(meld(h1, h2)) == findMin(h2)
    } else if (isEmpty(h2)) {
      findMin(meld(h1, h2)) == findMin(h1)
    } else {
      findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
    }
  }

  property("min of 111") = forAll { h: H =>
    findMin(deleteMin(insert(2, insert(1, insert(3, empty))))) == 2
  }
}
