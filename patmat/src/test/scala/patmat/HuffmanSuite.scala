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

  test("test times()") {
	assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
  }
  
  test("singleton") {
    assert(!singleton(List()))
    assert(singleton(List(Leaf('a',2))))
    assert(!singleton(List(Leaf('a',2), Leaf('b',3))))
  }
  
  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
    
    val leaflist1 = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(combine(leaflist1) === List(Leaf('x',4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5)))
    
    val leaflist2 = List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('z', 3), Leaf('x', 4))
    assert(combine(leaflist2) === List(Leaf('x',4),                                                                                      
    		Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('z',3),List('e', 't', 'z'),6)))
  }

  test("create code tree") {
    assert(createCodeTree(string2Chars("abc")) === Fork(Leaf('c',1),Fork(Leaf('b',1),Leaf('a',1),List('b', 'a'),2),List('b', 'a', 'c'),3))
    assert(createCodeTree(string2Chars("someText")) === Fork(Leaf('c',1),Fork(Leaf('b',1),Leaf('a',1),List('b', 'a'),2),List('b', 'a', 'c'),3))
  }
  
  test("find a french secret") {
    assert(decodedSecret === string2Chars("huffmanestcool"))
  }
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(frenchCode, encode(frenchCode)("huffmanestcool".toList)) === "huffmanestcool".toList)
    }
  }
  
  test("merge codetables") {
    assert(mergeCodeTables(List(('a', List(0,1,0,1))), List(('b', List(1,0)), ('a', List(1,1)))) === List(('b', List(1,0)), ('a', List(0,1,0,1,1,1))))
  }
  
  test("create code table") {
    new TestTrees {
      assert(convert(t1) === List(('b', List(1)), ('a', List(0))))
    }
  }
}

