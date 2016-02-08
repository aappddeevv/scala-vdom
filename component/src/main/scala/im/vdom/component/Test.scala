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
package component

import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.timers._
import scala.concurrent.{ Future, ExecutionContext }
import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import _root_.org.scalajs.{dom => d}
import d.document

import VNode._
import HTML5Attributes._
import Styles._

object Test extends JSApp {

  def main(): Unit = {
    println("test of scala-vdom component")

    case class CountState(val count: Int, val node: d.Node, val tree: VNode) extends State

    val comp = Component[CountState] { state =>
      val newCount = state.count + 1
      val render = tag("div", tag("button",
        text(s"Click Me - $newCount"), text(s"You have clicked the button $newCount times!")))
      (render, state.copy(tree = render, count = newCount))
    }
  }
}
