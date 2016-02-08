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

import scala.util._
import _root_.org.scalajs.{ dom => d }
import concurrent._
import scalajs.js
import js._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.scalatest.AsyncFlatSpec
import org.scalatest.Matchers
import org.scalatest.OptionValues

/**
 * Test rendering virtual nodes to DOM nodes..
 */
class PatchApplicableSpec extends AsyncFlatSpec
    with Matchers
    with OptionValues {

  import js.DynamicImplicits.truthValue
  import backend.dom.DOMBackend
  import DOMBackend._
  import backend.dom.DOMUtils
  import VNode._

  private def cleanBody() = DOMUtils.removeChildren(d.document.body)

  "Applying a patch" should "add an element" in {
    val vnode = tag("div")
    val body = d.document.body
    DOMUtils.removeChildren(body)
    body should not equal (null)
    val x = DOMBackend.run(InsertPatch(vnode)(body))
    x.map { newNode =>
      newNode.nodeName shouldEqual "DIV"
      body.childNodes.length shouldEqual 1
    }
  }

  it should "insert a vnode at a specific position" in {
    val body = DOMUtils.removeChildren(d.document.body)
    val node1 = d.document.createElement("h1")
    val node2 = d.document.createElement("h2")
    val vnode3 = tag("h3")
    val node4 = d.document.createElement("span")
    Seq(node1, node2, node4).foreach(body.appendChild(_))
    val x = DOMBackend.run(InsertPatch(vnode3, Some(2))(body))
    x.map { el =>
      body.childNodes(2).nodeName shouldEqual "H3"
    }
  }

  it should "add a 2 level div tree automatically" in {
    val body = d.document.body
    DOMUtils.removeChildren(body)
    val vnode = tag("div", tag("div"))
    val x = DOMBackend.run(InsertPatch(vnode)(body))
    x.map { el =>
      body.childNodes.length shouldEqual 1
      val div1 = body.childNodes(0)
      div1.nodeName should be("DIV")
      div1.childNodes.length shouldEqual 1
      val div2 = div1.childNodes(0)
      div2.nodeName should be("DIV")
      div2.childNodes.length shouldEqual 0
    }
  }

  it should "remove an element" in {
    val body = d.document.body
    DOMUtils.removeChildren(body)
    body.appendChild(d.document.createElement("div"))
    body.childNodes.length shouldEqual 1
    val x = DOMBackend.run(RemovePatch(body.childNodes(0)))
    x.map { el =>
      el should not be (null)
      el.nodeName shouldEqual "DIV"
    }
  }

  it should "replace an element" in {
    val body = DOMUtils.removeChildren(d.document.body)
    val vnode = tag("div", tag("div"))
    val replacement = tag("p")
    val y = for {
      newNode <- DOMBackend.run(InsertPatch(vnode)(body))
      replaced <- DOMBackend.run(ReplacePatch(replacement)(newNode))
    } yield replaced
    y.map { el =>
      el.nodeName shouldEqual "P"
      body.childNodes.length shouldEqual 1
    }
  }

  it should "reorder children correctly" in {
    val body = DOMUtils.removeChildren(d.document.body)
    val children = Seq("p", "h1", "h2", "span").map(d.document.createElement(_))
    val childrenNodeNames = children.map(_.nodeName)
    children.foreach(body.appendChild(_))
    (0 until body.childNodes.length).map(body.childNodes(_).nodeName) should contain theSameElementsInOrderAs childrenNodeNames
    body.childNodes.length shouldEqual children.length

    // Reorder only.
    val instr = ReorderInstruction(moves = Seq((0, 1), (1, 0)))
    val x = DOMBackend.run(OrderChildrenPatch(instr)(body)) map { el =>
      el.childNodes.length shouldEqual children.length
      (0 until el.childNodes.length).map(el.childNodes(_).nodeName) should contain theSameElementsInOrderAs Seq("H1", "P", "H2", "SPAN")
    }
    x
  }

  it should "remove and reorder children correctly" in {
    val body = DOMUtils.removeChildren(d.document.body)
    val children = Seq("p", "h1", "h2", "span").map(d.document.createElement(_))
    val childrenNodeNames = children.map(_.nodeName)
    children.foreach(body.appendChild(_))

    // Reorder and remove.
    val instr2 = ReorderInstruction(moves = Seq((2, 1), (1, 2)), removes = Seq(0))
    val y = DOMBackend.run(OrderChildrenPatch(instr2)(body)) map { el =>
      el.childNodes.length shouldEqual (children.length - 1)
      (0 until el.childNodes.length).map(el.childNodes(_).nodeName) should contain theSameElementsInOrderAs Seq("H1", "SPAN", "H2")
    }
    y
  }

  it should "add text from a text patch" in {
    val body = cleanBody()
    for {
      // we run it this way because we don't want to replace BODY, need a DIV
      // and then run the TextPatch
      x <- DOMBackend.run(InsertPatch(tag("div"))(body))
      y <- DOMBackend.run(TextPatch("my text")(x))
    } yield {
      val z: UndefOr[d.Text] = y.asInstanceOf[d.Text]
      assert(z.isDefined)
      z.get.data shouldEqual "my text"
    }
  }

}



