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
package backend
package dom

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.queue
import scala.scalajs.js
import scala.scalajs.js._
import scala.scalajs.js.UndefOr
import scala.scalajs.js.UndefOr._
import scala.language._

import _root_.org.scalajs.{dom => d}

import Defaults._


/**
 * Convert a virtual node to a DOM node.
 * 
 * TODO: Fix the execution context. Ensure that the render output future is
 * dependent on the children rendering futures.
 */
trait RenderToDOMComponent extends RendererComponent {
  self: Backend with AttributeComponent =>

  import DOMEnvironment._

  type RenderOutput = d.Node

  def render(vnode: VNode)(implicit executor: ExecutionContext): IOAction[RenderOutput] = {
    vnode match {
      case v@VirtualText(content) => Action.lift { createText(content) }

      case v@VirtualElementNode(tag, attributes, children, key, namespace) =>
        val makeEl: IOAction[d.Node] = Action.lift {
          val newNode = createEl(v)
          attributes.foreach(_(newNode))
          newNode
        } flatMap { el =>
          val childrenactions = children.map { child =>
            val renderChildAction = render(child)
            // a bit ugly since appendChild is really a side effoct
            renderChildAction.map { c => el.appendChild(c); c }
          }
          // the child render and append won't happen unless its exposed to be run later
          Action.seq(childrenactions: _*) andThen Action.successful(el)
        }
        makeEl

      case EmptyNode() =>
        /** Empty nodes become empty divs */
        Action.lift(createEl(VNode.tag("div")))

      case ThunkNode(f) => render(f())

      case CommentNode(c) =>
        Action.lift(d.document.createComment(c))
    }
  }
}

