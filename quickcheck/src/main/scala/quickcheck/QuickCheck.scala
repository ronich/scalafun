package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- Gen.oneOf(Gen.const(empty), genHeap)
  } yield insert(n, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("After inserting one element heap min should be that element") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("After inserting one element and deleting min heap should be empty") = forAll { a: A =>
    val h = insert(a, empty)
    deleteMin(h)
    isEmpty(h)
  }

  property("After inserting two elements min should be the smaller one") = forAll { (a: A, b: A) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == min(a, b)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

}
