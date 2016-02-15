/* Copyright 2015 Devon Miller
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at9
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
package backend
package dom

import scala.language._
import org.scalatest._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import _root_.org.scalajs.dom.{ document => d, Element => El }

/**
 * DOMUtils test.
 */
class DOMUtilsSpec extends FlatSpec with Matchers with OptionValues {

  import Utils._
  import DOMUtils._

  "canReuseMarkup" should "return true when it should return true" in {
    val n = d.createElement("div")
    val markup = """<div>blah is cool</div>"""
    val markupWith = """<div data-scala-vdom-checksum="1628178442">blah is cool</div>"""
    n.innerHTML = markupWith
    canReuseMarkup(markup, n.firstChild.asInstanceOf[El]) should equal(true)
  }
  
  "removeChildren" should "remove all children" in { 
    val n = d.createElement("div")
    n.innerHTML = """<div></div><p>hello</p><h1>world</h1>"""
    n.childNodes.length should be >(0)
    removeChildren(n)
    n.childNodes.length should be (0)
    
  }

}
