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

import scala.language._
import org.scalatest._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import vdom.VNode._

class RenderMarkupSpec extends FlatSpec
    with Assertions
    with Matchers
    with OptionValues {

  val b = MarkupBackend

  import HTML5Attributes._
  import Styles._

  "Backend" should "render markup" in {
    val vdom = vnode("div", Seq(cls := "highlighted", data("hah") := "blah", heightA := None,
      width := 30, border := "auto", textAlign := "left"),
      vnode("p", text("blah")), vnode("br"))
    val action = b.render(vdom)
    val output = b.run(action)
    val markup = Await.result(output, 1 seconds)
    println(s"markup: $markup")
  }

}