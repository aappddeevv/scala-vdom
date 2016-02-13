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

import _root_.org.scalajs.{ dom => d }
import scalajs.js

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.OptionValues

/**
 * Test cleanup actions on DOM nodes and properties.
 */
class CleanupSpec extends FlatSpec
    with Matchers
    with OptionValues {

  import d.document
  import backend.dom.DOMBackend
  import js.DynamicImplicits.truthValue

  "cleanup component" should "attach a cleanup queue to a DOM node" in {
    val p = document.createElement("p")
    DOMBackend.addDetachAction(p, Action.successful(true))
    truthValue(p.asInstanceOf[js.Dynamic].__namedCleanupActions) should be(true)
  }

  it should "run the node level cleanup action when asked to cleanup" in {
    val p = document.createElement("p")
    var counter = 0
    DOMBackend.addDetachAction(p, Action.lift {
      counter += 1
    })
    DOMBackend.runActions(p)
    counter should be(1)
  }

  it should "allow a named cleanup queue to be created" in {
    val p = document.createElement("p")
    var counter = 0
    DOMBackend.addAction("blah", p, Action.lift {
      counter += 1
    })
    val t = p.asInstanceOf[js.Dynamic].__namedCleanupActions.asInstanceOf[Map[String, IOAction[_]]]
    t.contains("blah") should be(true)

    DOMBackend.runActions(p)
    counter should be(1)
  }

  it should "process the hierachy of nodes and run all detach actions" in {
    val div = document.createElement("div")
    var counter = 0
    DOMBackend.addDetachAction(div, Action.lift {
      counter += 1
    })
    val div2 = document.createElement("p")
    DOMBackend.addDetachAction(div2, Action.lift {
      counter += 1
    })
    div.appendChild(div2)

    DOMBackend.runActions(div)
    counter should be (2)
  }

}
