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

import scala.util._
import org.im.vdom.backend._
import org.im.vdom.VNode._

import org.scalajs.dom
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
  import DOMBackend._

  "Applying a patch" should "add an element" in {
    val vnode = tag("div")
    val body = dom.document.body
    DOMUtils.removeChildren(body)
    body should not equal (null)
    val x = DOMBackend.run(InsertPatch(vnode)(body))
    x.map { newNode =>
      newNode.nodeName shouldEqual "DIV"
      body.childNodes.length shouldEqual 1
    }
  }

  it should "add a 2 level div tree automatically" in {
    val body = dom.document.body
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
    val body = dom.document.body
    DOMUtils.removeChildren(body)
    body.appendChild(dom.document.createElement("div"))
    body.childNodes.length shouldEqual 1
    val x = DOMBackend.run(RemovePatch(body.childNodes(0)))
    x.map { el =>
      el should not be (null)
      el.nodeName shouldEqual "DIV"
    }
  }

  it should "replace an element" in {
    val body = DOMUtils.removeChildren(dom.document.body)
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
    val body = DOMUtils.removeChildren(dom.document.body)
    val p = dom.document.createElement("p")
    val h1 = dom.document.createElement("h1")
    Seq(p, h1).foreach(body.appendChild(_))
    body.childNodes(0).nodeName shouldEqual "P"
    body.childNodes(1).nodeName shouldEqual "H1"
    body.childNodes.length shouldEqual 2
    val instr = ReorderInstruction(moves = Seq((0, 1), (1, 0)))
    val x = DOMBackend.run(OrderChildrenPatch(instr)(body))
    x.map { el =>
      el.childNodes.length shouldEqual 2
      el.childNodes(0).nodeName shouldEqual "H1"
      el.childNodes(1).nodeName shouldEqual "P"
    }
  }
}
