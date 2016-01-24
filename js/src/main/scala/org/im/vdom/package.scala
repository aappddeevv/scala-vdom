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

import scala.language._

import scalajs.js
import js.UndefOr
import UndefOr._
import org.scalajs.dom

trait Defaults {

  /**
   * Convenience methods for DOM Nodes and Elements.
   */
  implicit class RichNode[T <: dom.Node](n: T) {
    import UndefOr._
    def parentUndefOr: UndefOr[dom.Node] = {
      val p = n.parentNode
      if (p == null) js.undefined
      p
    }
  }

}

object Defaults extends Defaults

/**
 * KeyValues that holds a few pieces of function configuration
 * information relevant to calling a handler when a dom.Event is fired.
 */
case class FunctionValue(handler: events.Handler, matcher: events.Matcher = events.Matcher.MatchRoot,
  useCapture: Option[Boolean] = None)

/**
 * Value that is a function for an event handler. The entire
 * event handling capabilities need to be lifted to a string
 * based action framework to allow server side rendering to
 * work better on jvm. Need to think this through for awhile.
 */
case class FunctionKey(val name: String) extends KeyPart { self =>
  def ~~>(v: events.Handler) = KeyValue[FunctionValue](self, Some(FunctionValue(v, events.Matcher.MatchRoot, None)))
  def ~~>(v: FunctionValue) = KeyValue[FunctionValue](self, Some(v))
}

/**
 * Convenience class to allow you to define a FunctionKey using '"keyname".func`.
 */
class RichFuncString(val name: String) {
  def func = FunctionKey(name)
}