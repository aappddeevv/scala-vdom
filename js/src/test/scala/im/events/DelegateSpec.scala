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
package events

import events._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.OptionValues
import _root_.org.scalajs.{dom => d}

/**
 * Test Delegates.
 */
class DelegateSpec extends FlatSpec
    with Matchers
    with OptionValues {

  import d.document
  import vdom.backend.dom.DOMBackend
  import scala.scalajs.js.DynamicImplicits.truthValue

  "delegate component" should "attach a Delegate to a node" in {
    val p = document.createElement("p")
    val d = Delegate(None)
    DOMBackend.linkDelegate(d, p)
    DOMBackend.getDelegate(p) should not be (null)
  }

  it should "allow removing the Delegate" in {
    val p = document.createElement("p")
    val d = Delegate(None)
    DOMBackend.linkDelegate(d, p)
    DOMBackend.getDelegate(p) should not be (null)
    DOMBackend.rmDelegate(p)
    DOMBackend.getDelegate(p) should be(None)
  }

}