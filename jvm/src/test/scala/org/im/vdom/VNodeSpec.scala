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
package org.im
package vdom

import org.scalatest._

/**
 * Patches do not need much testing...so this is really
 * just a dummy test class.
 */
class VNodeSpec extends FlatSpec
    with Matchers
    with OptionValues
    with prop.PropertyChecks {

  //import Matchers._
  import vdom.VNode._

  "VNode" should "have equals that works" in {
    val lhs = tag("div", Some("key1"), None, Seq())
    val rhs = tag("div", Some("key1"), None, Seq())
    val rhs2 = tag("div", Some("key2"), None, Seq())

    assertResult(true)(lhs == rhs)
    assertResult(false)(rhs == rhs2)
  }

  it should "equals on a simple tag" in {
    val lhs = tag("div")
    val rhs = tag("div")
    assertResult(true)(lhs == rhs)
    assertResult(false)(lhs eq rhs)
  }

  "closeTo" should "test on tag, key and namespace" in {
    val lhs = tag("div", None, None, Seq())
    val rhs = tag("div", None, None, Seq())
    assertResult(true)(lhs.closeTo(rhs))

    val lhs1 = tag("div", Some("k1"), None, Seq())
    val rhs1 = tag("div", Some("k1"), None, Seq())
    assertResult(true)(lhs1.closeTo(rhs1))

    val rhs2 = tag("div", None, None, Seq())
    assertResult(false)(lhs1.closeTo(rhs2))

    val lhs3 = tag("div", Some("k1"), Some("ns"), Seq())
    val rhs3 = tag("div", Some("k1"), Some("ns"), Seq())
    assertResult(true)(lhs3.closeTo(rhs3))

    val rhs4 = tag("div", Some("k1"), None, Seq())
    assertResult(false)(lhs3.closeTo(rhs4))
  }

  "closeToOrEqual" should "use closeTo when a VirtualElementNode and == when anything else" in {
    import VNodeUtils.closeToOrEquals
    val lhs = text("test")
    val rhs = text("test")
    assertResult(true)(closeToOrEquals(lhs, rhs))

    val rhs2 = comment("comment")
    assertResult(false)(lhs.closeToOrEquals(rhs2))

    val lhs3 = tag("div", Some("k1"), Some("ns"), Seq())
    val rhs3 = tag("div", Some("k1"), Some("ns"), Seq())
    assertResult(true)(closeToOrEquals(lhs3, rhs3))

    assertResult(false)(closeToOrEquals(lhs, rhs3))
  }

  "kindOfContains" should "find something close in the list" in {
    val seq = Seq(text("text"), tag("div", Some("k1"), None, Seq()), comment("comment"))
    assertResult(true)(VNodeUtils.kindOfContainsCloseEnough(seq, tag("div", Some("k1"), None, Seq())))
    assertResult(true)(VNodeUtils.kindOfContainsCloseEnough(seq, comment("comment")))
    assertResult(false)(VNodeUtils.kindOfContainsCloseEnough(seq, tag("div", Some("k1"), Some("ns"), Seq())))
  }

  "scalacheck" should "run" in {
    forAll { (n: Int) =>
      whenever(n > 0) {
        n should be > 0
      }
    }
  }

  class Fraction(n: Int, d: Int) {

    require(d != 0)
    require(d != Integer.MIN_VALUE)
    require(n != Integer.MIN_VALUE)

    val numer = if (d < 0) -1 * n else n
    val denom = d.abs

    override def toString = numer + " / " + denom
  }

  val invalidCombos =
    Table(
      ("n", "d"),
      (Integer.MIN_VALUE, Integer.MIN_VALUE),
      (1, Integer.MIN_VALUE),
      (Integer.MIN_VALUE, 1),
      (Integer.MIN_VALUE, 0),
      (1, 0))

  "scalacheck2" should "run" in {
    //    forAll(invalidCombos) { (n: Int, d: Int) =>
    //      evaluating {
    //        new Fraction(n, d)
    //      } should produce[IllegalArgumentException]
    //    }
  }

  "findRemoves" should "find removes in easy vnode list" in {
    val lhs = Seq(text("t1"), text("t2"), text("t3"))
    val rhs = Seq(text("t1"), text("t3"))
    val r = VNodeUtils.findRemovesCloseEnough(lhs, rhs)
    assertResult(1)(r.size)
    assertResult(1)(r(0))
  }

  it should "not remove nodes if they are close to each other with keys" in {
    val lhs = Seq(text("t1"), tag("div", Some("k1"), None, Seq(), text("text")), text("t3"))
    val rhs = Seq(text("t1"), tag("div", Some("k1"), None, Seq()), text("t3"))
    val r = VNodeUtils.findRemovesCloseEnough(lhs, rhs)
    assertResult(0)(r.size)
  }

  it should "not remove nodes if they are close to each without keys" in {
    val lhs = Seq(text("t1"), tag("div", None, None, Seq(), text("text")), text("t3"))
    val rhs = Seq(text("t1"), tag("div", None, None, Seq()), text("t3"))
    val r = VNodeUtils.findRemovesCloseEnough(lhs, rhs)
    assertResult(0)(r.size)
  }

  it should "not find any removes on empty lists" in {
    val rhs: Seq[VNode] = Seq.empty
    val lhs: Seq[VNode] = Seq.empty
    assertResult(0)(VNodeUtils.findRemovesCloseEnough(lhs, rhs).size)
  }

  it should "find 1 remove when the target is empty and source has 1 el" in {
    val lhs = Seq(text("t"))
    assertResult(1)(VNodeUtils.findRemovesCloseEnough(lhs, Seq.empty).size)
  }

  it should "find 1 remove when the target has 1 el different from the source" in {
    val rhs = Seq(text("tnew"))
    val lhs = Seq(text("told"))
    assertResult(1)(VNodeUtils.findRemovesCloseEnough(lhs, rhs).size)
  }

  "findAdds" should "find adds in easy vnode list" in {
    val rhs = Seq(text("t1"), text("t2"), text("t3"))
    val lhs = Seq(text("t1"), text("t3"))
    val r = VNodeUtils.findAddsCloseEnough(lhs, rhs)
    assertResult(1)(r.size)
    assertResult(1)(r(0))
  }

}