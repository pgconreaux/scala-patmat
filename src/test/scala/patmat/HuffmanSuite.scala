package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val t3 = Leaf('b', 3)

    val cd = makeCodeTree(Leaf('c', 1), Leaf('d', 1))
    val ef = makeCodeTree(Leaf('e', 1), Leaf('f', 1))
    val gh = makeCodeTree(Leaf('g', 1), Leaf('h', 1))

    val bcd = makeCodeTree(Leaf('b', 3), cd)
    val efgh = makeCodeTree(ef, gh)

    val bcdefgh = makeCodeTree(bcd, efgh)

    val abcdefgh = makeCodeTree(Leaf('a', 8), bcdefgh)

    val table = List(('b', List(1, 0, 0)), ('a', List(0)), ('g', List(1, 1, 1, 0)))

  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("weight of a leaf") {
    new TestTrees {
      assert(weight(t3) === 3)
    }
  }

  test("chars of a leaf") {
    new TestTrees {
      assert(chars(t3) === List('b'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"hello, world\")") {
    val chars = (string2Chars("hello, world"))
    //println(times(chars))
    assert(times(chars) === List(('d', 1), ('l', 3), ('r', 1), ('o', 2), ('w', 1), (' ', 1), (',', 1), ('e', 1), ('h', 1)))
  }

  test("times(\"hello, goodbye\")") {
    val chars = (string2Chars("hello, goodbye"))
    //println(times(chars))
    assert(times(chars) === List(('e', 2), ('y', 1), ('b', 1), ('d', 1), ('o', 3), ('g', 1), (' ', 1), (',', 1), ('l', 2), ('h', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    //println (makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }
  
    test("makeOrderedLeafList single element") {
    //println (makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))
    assert(makeOrderedLeafList(List(('e', 1))) === List(Leaf('e', 1)))
  }

  test("singleton for empty list") {
    assert(singleton(List()) === false)
  }

  test("singleton for non-empty list of three leaf elems") {
    assert(singleton(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3))) === false)
  }

  test("singleton for non-empty list of one leaf elem") {
    assert(singleton(List(Leaf('e', 1))) === true)
  }

  test("singleton for non-empty list of one fork elem") {
    assert(singleton(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3))) === true)
  }

  test("singleton for non-empty list of one fork and one leaf elem") {
    assert(singleton(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4))) === false)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

//  test("combine of some fork and leaf list") {
//    val leaflist = List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), Leaf('y', 5))
//    assert(combine(leaflist) === List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7), Leaf('y', 5)))
//  }

  test("combine of empty list") {
    val leaflist = List()
    assert(combine(leaflist) === List())
  }

  test("combine of singleton list") {
    val leaflist = List(Leaf('e', 1))
    assert(combine(leaflist) === List(Leaf('e', 1)))
  }
  
  test("combine on long text") {
    val msg = "The Huffman encoding of this message should be three hundred and fifty-two bits long".toList
    assert(encode(createCodeTree(msg))(msg).size === 352)
  }

  test("until") {
    val trees = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val combined = until(singleton, combine)(trees)
    println(combined)
    assert(singleton(combined))
  }

  test("until on singleton list") {
    val trees = List(Leaf('e', 1))
    val combined = until(singleton, combine)(trees)
    println(combined)
    assert(singleton(combined))
  }

  test("until on empty list") {
    val trees = List()
    val combined = until(singleton, combine)(trees)
    println(combined)
    assert(!singleton(combined))
  }

  test("create code tree") {
    val chars = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    println(makeOrderedLeafList(times(chars)))
    val tree = createCodeTree(chars)
    println(tree)
  }

  test("decode t1") {
    new TestTrees {
      assert(decode(t1, List(0, 1)) === "ab".toList)
    }
  }

  test("decode t2") {
    new TestTrees {
      assert(decode(t2, List(0, 1, 0, 0, 1)) === "bad".toList)
    }
  }

  test("decode empty bits list") {
    new TestTrees {
      assert(decode(t2, List()) === "".toList)
    }
  }

  test("decodedSecret") {
    println(decodedSecret)
    assert(decodedSecret == "huffmanestcool".toList)
  }

  test("encode t2") {
    new TestTrees {
      println(encode(t2)("bad".toList))
      assert((encode(t2)("bad".toList)) === List(0, 1, 0, 0, 1))
      //assert(encode(t2, List(0, 1, 0, 0, 1)) === "bad".toList)
    }
  }

  test("encode abcdefgh") {
    new TestTrees {
      println(encode(abcdefgh)("bag".toList))
      assert((encode(abcdefgh)("bag".toList)) === List(1, 0, 0, 0, 1, 1, 1, 0))
      //assert(encode(t2, List(0, 1, 0, 0, 1)) === "bad".toList)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits") {
    new TestTrees {
      assert(codeBits(table)('b') === List(1, 0, 0))
      assert(codeBits(table)('a') === List(0))
      assert(codeBits(table)('g') === List(1, 1, 1, 0))
    }
  }

  test("codeBits with empty") {
    new TestTrees {
      assert(codeBits(Nil)('b') === List())
    }
  }

  test("mergeCodeTables left only - leaf") {
    val a = List(('e', Nil), ('f', Nil))
    println(mergeCodeTables(a, Nil))
  }

  test("mergeCodeTables left only - fork") {
    val a = List(('e', List(0)), ('f', List(1)))
    println(mergeCodeTables(a, Nil))
  }

  test("mergeCodeTables right only - leaf") {
    val b = List(('g', Nil), ('h', Nil))
    println(mergeCodeTables(Nil, b))
  }

  test("mergeCodeTables right only - fork") {
    val b = List(('g', List(0)), ('h', List(1)))
    println(mergeCodeTables(Nil, b))
  }

  test("mergeCodeTables right and left") {
    val a = List(('e', List(0)), ('f', List(1)))
    val b = List(('g', List(0)), ('h', List(1)))
    println(mergeCodeTables(a, b))
  }

  test("mergeCodeTables right and left - tree") {
    val e = List(('e', Nil))
    val f = List(('f', Nil))
    val g = List(('g', Nil))
    val h = List(('h', Nil))
    val ef = mergeCodeTables(e, f)
    val gh = mergeCodeTables(g, h)
    val efgh = mergeCodeTables(ef, gh)
    println(efgh)
  }

  test("convert t1") {
    new TestTrees {
      println(convert(t1))
    }
  }

  test("convert t2") {
    new TestTrees {
      println(convert(t2))
    }
  }

  test("convert t3") {
    new TestTrees {
      println(convert(t3))
    }
  }

  test("convert abcdefgh") {
    new TestTrees {
      println(convert(abcdefgh))
    }
  }

  test("quickEncode abcdefgh bag") {
    new TestTrees {
      println(quickEncode(abcdefgh)("bag".toList))
      assert((quickEncode(abcdefgh)("bag".toList)) === List(1, 0, 0, 0, 1, 1, 1, 0))
      //assert(encode(t2, List(0, 1, 0, 0, 1)) === "bad".toList)
    }
  }
  
    test("quickEncode abcdefgh bac") {
    new TestTrees {
      println(quickEncode(abcdefgh)("bac".toList))
      assert((quickEncode(abcdefgh)("bac".toList)) === List(1, 0, 0, 0, 1, 0, 1, 0))
      //assert(encode(t2, List(0, 1, 0, 0, 1)) === "bad".toList)
    }
  }

}
