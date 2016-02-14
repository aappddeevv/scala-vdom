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

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

/**
 * Render to HTML strings.
 *
 */
trait MarkupRendererComponent extends RendererComponent {
  self: Backend with DOMHints =>

  import Utils._

  type RenderOutput = String

  def render(vnode: VNode)(implicit executor: ExecutionContext): IOAction[RenderOutput] = {
    val ctx = createContext().getEC(executor)

    vnode match {
      case v@VirtualText(content) =>
        Action.successful(content)

      case v@VirtualElementNode(tag, attributes, children, key, namespace) =>

        // find style "attributes", then add
        val processStyles = Action.lift {          
          val valstr = attributes.filter(keepStyles).map { kv =>
            createMarkupForStyles(kv, styleHint(kv.key.name)).getOrElse("")
          }.mkString(";")
          valstr match { 
            case "" => ""
            case v => s"""style="$v;""""
          }
        }

        // process remaining attributes
        val processAttributes = Action.lift {
          attributes.filter(keepAttributes).map { kv =>
            createMarkupForProperty(kv, attrHint(kv.key.name)).getOrElse("")
          }.mkString(" ")
        }

        // create tag content
        val childMarkup = Action.fold(children.map { child =>
          render(child)
        }, "") { stringAppend }

        val elHint = self.elHint(tag)

        // If omit closing tag, just close tag, don't generate middle content.
        val middleEnd = elHint.filter(h => h.values(Hints.OmitClosingTag)).fold {
          Seq(Action.successful(">"),
            childMarkup,
            Action.successful("</" + tag + ">"))
        } { h => Seq(Action.successful("/>")) }

        Action.fold(Seq(Action.successful("<" + tag + " "),
          processStyles,
          processAttributes) ++ middleEnd, "") { stringAppend }

      case EmptyNode() =>
        Action.successful("<div></div>")
      case CommentNode(content) =>
        Action.successful("<!-- " + content + " -->")
      case ThunkNode(f) =>
        render(f())
      case x@_ =>
        Action.failed(new VDomException(s"Unknown VNode type $x for $this"))
    }
  }
}

trait MarkupBackend extends Backend with MarkupRendererComponent with DOMHints {
  type This = MarkupBackend
  type Context = BasicContext

  protected[this] def createContext() = new BasicContext {}
}

object MarkupBackend extends MarkupBackend
