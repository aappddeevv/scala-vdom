/**
 * Copyright 2015 Devon Miller
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package im
package vdom

import org.scalatest._

/**
 * Test diffing algorithm.
 */
class DiffSpec extends FlatSpec
    with Matchers
    with OptionValues {

  import VNode._
  import VNodeUtils._

  val nodea = Seq(
    tag("li", text("one")),
    tag("li", text("two")),
    tag("li", text("three")))

  val nodeb = Seq(
    tag("li", text("one")),
    tag("li", text("two")),
    tag("li", text("three")),
    tag("li", text("four")))

  val nodeBChangeLastVNode = Seq(
    tag("li", text("one")),
    tag("li", text("two")),
    tag("li", text("three")),
    tag("p"))

  "General vnode tests" should "show two lists of line items to be not equal if they have different content" in {
    nodea should not equal (nodeb)
  }

  it should "show two ul with the same number of children to be close to but may not be equal" in {
    val nodea = tag("ul",
      tag("li", text("foo")),
      tag("li", text("fi")),
      tag("li", text("fee")))
    val nodeb = tag("ul",
      tag("li", text("foo")),
      tag("li", text("fi")),
      tag("li", text("fee")))
    assert(nodea closeTo nodeb)
  }

  it should "two ul with the same number of children but different keys to not be close to" in {
    val nodea = tag("ul", "key1", Seq(),
      tag("li", text("foo")),
      tag("li", text("fi")),
      tag("li", text("fee")))
    val nodeb = tag("ul", "key2", Seq(),
      tag("li", text("foo")),
      tag("li", text("fi")),
      tag("li", text("fee")))
    nodea closeTo nodeb should not be (true)
  }

  it should "correctly find or not find a vnode in a list of vnodes using kindOfContains equals and closeEnough" in {
    val x = kindOfContains(nodeb, nodeb(3))(_ == _)
    x should be(true)

    val y = kindOfContainsCloseEnough(nodeb, nodeb(3))
    y should be(true)

    val z = kindOfContains(nodeb, tag("li", text("boom")))(_ == _)
    z should be(false)

    val ztoo = kindOfContainsCloseEnough(nodeb, tag("li", text("boom")))
    ztoo should be(true)

    val znot = kindOfContainsCloseEnough(nodeb, tag("li"))
    znot should be(true)
  }

  it should "find removes using equality but not find any using closeEnough when using similar vnodes" in {
    val x = findRemoves(nodeb, nodea)(_ == _)
    x should have length 1
    x should contain(3)

    val y = findRemovesCloseEnough(nodeb, nodea)
    withClue("find removes using closeEnough") {
      y should have length 0
    }
  }

  it should "find removes using equality and closeEnough when using non-similar vnodes" in {
    val x = findRemoves(nodeBChangeLastVNode, nodea)(_ == _)
    x should have length 1
    x should contain(3)

    val y = findRemovesCloseEnough(nodeBChangeLastVNode, nodeb)
    withClue("find removes using closeEnough") {
      y should have length 1
    }
  }

  it should "not find removes comparing with equality and closeEnough and comparing a sequence to itself" in {
    val x = findRemoves(nodeb, nodeb)(_ == _)
    x should have length 0

    val y = findRemovesCloseEnough(nodeb, nodeb)
    withClue("find removes using closeEnough") {
      y should have length 0
    }
  }

  it should "find adds using exact matches but not with closeEnough" in {
    val x = findAdds(nodea, nodeb)(_ == _)
    x should have length 1
    x(0) should equal(3)

    val y = findRemovesCloseEnough(nodea, nodeb)
    withClue("find adds using closeEnough") {
      y should have length 0
    }
  }

  "diffSeq" should "should find the removes when one vnode is removed in a sequence" in {
    val (moves, removes, adds, rest) = Diff.diffSeq2(nodeb, nodea, Nil)
    removes should have length 1
  }

  it should "should find the adds when one vnode is removed in a sequence" in {
    val (moves, removes, adds, rest) = Diff.diffSeq2(nodea, nodeb, Nil)
    adds should have length 1
  }

  it should "find simple moves if two children swap places" in {
    val lhs = Seq(nodea(0), nodea(1))
    val rhs = Seq(nodea(1), nodea(0))
    val (moves, removes, adds, rest) = Diff.diffSeq2(lhs, rhs, Nil)
    withClue("moves") { moves should have length 2 }
    withClue("removes") { removes should have length 0 }
    withClue("adds") { adds should have length 0 }
  }

  it should "find simple moves if two children change places" in {
    val lhs = Seq(nodea(0), nodea(1), nodea(2))
    val rhs = Seq(nodea(2), nodea(1), nodea(0))
    val (moves, removes, adds, rest) = Diff.diffSeq2(lhs, rhs, Nil)
    withClue("moves") { moves should have length 2 }
    withClue("removes") { removes should have length 0 }
    withClue("adds") { adds should have length 0 }
  }

  it should "find simple removes and moves if two children change place and one child is dropped" in {
    val lhs = Seq(nodea(0), nodea(1), nodea(2))
    val rhs = Seq(nodea(2), nodea(1))
    val (moves, removes, adds, rest) = Diff.diffSeq2(lhs, rhs, Nil)
    withClue("moves") {
      moves should have length 2
      moves(0) should equal((1, 0))
      moves(1) should equal((0, 1))
    }
    withClue("removes") {
      removes should contain only (0)
    }
    withClue("adds") {
      adds should have length 0
    }
  }

  it should "find simple adds and moves if two children change place and one child is dropped" in {
    val lhs = Seq(nodea(0), nodea(1))
    val rhs = Seq(nodea(1), nodea(0), nodea(2))
    val (moves, removes, adds, rest) = Diff.diffSeq2(lhs, rhs, Nil)
    withClue("moves") {
      moves should have length 2
      moves(0) should equal((1, 0))
      moves(1) should equal((0, 1))
    }
    withClue("removes") {
      removes should have length 0
    }
    withClue("adds") {
      adds should contain only (2)
    }
  }

  "Diff" should "create an empty patch of both vnodes are the same" in {
    val nodea = tag("p")
    val nodeb = tag("p")
    val d = diff(nodea, nodeb)
    d should equal(EmptyPatch)
  }

  it should "create an empty patch with a multi-level vnode tree when the trees are the same" in {
    val d = diff(tag("ul", nodea: _*), tag("ul", nodea: _*))
    d should equal(EmptyPatch)
  }

}
