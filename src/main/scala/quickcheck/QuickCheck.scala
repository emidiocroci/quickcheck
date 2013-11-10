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

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    insert(b, h) 
    findMin(h) == scala.math.min(a, b)
  }

  property("delMin") = forAll { a: Int =>
  	val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }
  
  property("minMeld") = forAll { (a: H, b: H) =>
 	  val melded = meld(a, b)
    print(findMin(melded) + " " + findMin(a) + " " + findMin(b))
 	  findMin(melded) == findMin(a) || findMin(melded) == findMin(b)
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
    x <- scala.util.Random.nextInt()
    item <- oneOf(empty, genHeap)
  } yield meld(empty,insert(x, empty))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
