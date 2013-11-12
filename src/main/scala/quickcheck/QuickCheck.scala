package quickcheck
// > <console>:1: error: illegal start of definition
// >        package quickcheck
// >        ^

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }
  
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert( b, insert(a, empty))    
    findMin(h) == scala.math.min(a, b)
  }

  property("delMin2") = forAll { (a: Int, b: Int) =>
    val h = insert( b, insert(a, empty))        
    findMin(deleteMin(h)) == scala.math.max(a, b)
  }

  property("delMin") = forAll { a: Int =>
  	val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }
  
  property("minMeld") = forAll { (a: H, b: H) =>
 	  val min = findMin(meld(a, b))
 	  (min == findMin(a)) || (min == findMin(b))
  }    

  property("delMin") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(meld(h, empty)))
  }

  property("sortedHeap") = forAll { a: H =>
    def isSorted(min: Int, h: H): Boolean = 
      if (isEmpty(h))
        true
      else if( min > findMin(h) )
        false
      else
        isSorted(findMin(h), deleteMin(h))

    isSorted(findMin(a), deleteMin(a))
  }

  lazy val genHeap: Gen[H] = for {    
    x <- arbitrary[Int]
    item <- oneOf(empty, genHeap)
  } yield insert(x, item)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
