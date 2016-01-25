/* Copyright 2015 Devon Miller
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
package backend

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.language._

import org.im.vdom.HTML5Attributes
import org.im.vdom.Styles
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.OptionValues

import vdom.VNode.tag
import vdom.VNode.text

class RenderMarkupSpec extends FlatSpec with Matchers
    with OptionValues {

  val b = MarkupBackend

  import HTML5Attributes._
  import Styles._

  
  "Backend" should "render markup" in {
//    val vdom = tag("div", Seq(cls := "highlighted", data("hah") := "blah", heightA := None,
//      width := 30, border := "auto", textAlign := "left"),
//      tag("p", text("blah")), tag("br"))
//    val action = b.render(vdom)
//    val output = b.run(action)
//    val markup = Await.result(output, 1 seconds)
//    println(s"markup: $markup")
  }

}