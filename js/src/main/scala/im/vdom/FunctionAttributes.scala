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

import scala.language._

/**
 * DOM events. If you can define your own convenience methods for defining methods and import
 * those into scope.
 */
trait UIEvents {
  implicit class FunctionToKey(eventType: String) extends RichFuncString(eventType)
  val abort = "abort".func
  val beforeinput = "beforeinput".func
  val blur = "blur".func
  val click = "click".func
  val compositionstart = "compositionstart".func
  val compositionupdate = "compositionupdate".func
  val compositionend = "compositionend".func
  val dblclick = "dblclick".func
  val error = "error".func
  val focus = "focus".func
  val focusin = "focusin".func
  val focusout = "focusout".func
  val input = "input".func
  val keyup = "keyup".func
  val keydown = "keydown".func
  val load = "load".func
  val mousedown = "mousedown".func
  val mouseenter = "mouseenter".func
  val mouseleave = "mouseleave".func
  val mouseout = "mouseout".func
  val mouseover = "mouseover".func
  val resize = "resize".func
  val scroll = "scroll".func
  val select = "select".func
  val unload = "unload".func
  val wheel = "wheel".func
}

object UIEvents extends UIEvents

