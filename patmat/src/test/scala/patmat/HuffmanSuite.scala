package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

    val t3 = Fork(
              Leaf('a',8),
              Fork(
                Fork(
                  Leaf('b',3),
                  Fork(Leaf('c',1),Leaf('d',1),List('c','d'),2),
                  List('b','c','d'),
                  5
                ),
                Fork(
                 Fork(Leaf('e',1),Leaf('f',1),List('e','f'),2),
                 Fork(Leaf('g',1),Leaf('h',1),List('g','h'),2),
                 List('e','f','g','h'),
                 4
                ),
                List('b','c','d','e','f','g','h'),
                9
              ),
              List('a','b','c','d','e','f','g','h'),
              17
            )
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times"){
    assert(times(List('a','b','c','a','b','a')) === List(('a',3),('b',2),('c',1)))
  }

  test("times: with space"){
    assert(times(List('a','b','c','a','b','a',' ',' ',' ',' ')) === List((' ',4),('a',3),('b',2),('c',1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton: true"){
    assert(singleton(List(Leaf('e',1))))
  }

  test("singleton: false"){
    assert(!singleton(List(Leaf('e',1),Leaf('f',1))))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine: empty list"){
    assert(combine(List.empty) === List.empty)
  }

  test("combine: list with 2 elements"){
    assert(combine(List(Leaf('a',1),Leaf('b',2))) === List(Fork(Leaf('a',1),Leaf('b',2),List('a','b'),3)))
  }

  test("encode: assignment example - encode BAC"){
    new TestTrees {
      assert(encode(t3)(List('b','a','c')) === List(1,0,0,0,1,0,1,0))
    }
  }

  test("encodeChar: assignment example - encode B"){
    new TestTrees {
      assert(encodeChar(t3,'b',List.empty) === List(1,0,0))
    }
  }

  test("decode: assignment example - decode to BAC"){
    new TestTrees {
      assert(decode(t3,List(1,0,0,0,1,0,1,0)) === List('b','a','c'))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decodeSecret"){
    assert(decodedSecret == "huffmanestcool".toList)
  }

}
