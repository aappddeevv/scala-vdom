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

/**
 * Various constants related to DOM.
 */
trait Constants {

  /**
   * Namespace URIs
   */
  object NS {
    val HTML5 = "http://www.w3.org/1999/xhtml"
    val SVG = "http://www.w3.org/2000/svg"
    val XLINK = "http://www.w3.org/1999/xlink"
    val XML = "http://www.w3.org/XML/1998/namespace"
  }
}

object Constants extends Constants

trait HTML5Attributes {

  implicit class StandardStringToKey(name: String) extends RichString(name)

  val acceptCharSet = "accept-charset".attr
  val checked = "checked".attr
  val `class` = "class".attr
  val cls = `class`
  var contentEditable = "contenteditable".attr
  val contextMenu = "contextmenu".attr
  val dir = "dir".attr
  val hidden = "hidden".attr
  val href = "href".attr
  val httpEquiv = "http-equiv".attr
  val htmlFor = "for".attr
  val innerHTML = "innerHTML".attr
  val id = "id".attr
  val lang = "lang".attr
  val selected = "selected".attr
  val style = "style".attr
  val tabIndex = "tabindex".attr
  val title = "title".attr
  val value = "value".attr
  val widthA = "width".attr // conflicts with style
  val heightA = "height".attr // conflicts with style
}

trait CustomHTML5Attributes {
  implicit class CustomStringToKey(name: String) extends RichString(name)
  def data(name: String) = AttrKey("data-" + name)
  def aria(name: String) = AttrKey("aria-" + name)
}

object CustomHTML5Attributes extends CustomHTML5Attributes

/**
 * Standard HTML5 attributes. Usually, you'll import this.
 */
object HTML5Attributes extends HTML5Attributes with CustomHTML5Attributes

trait SVGAttributes {
  import Constants.NS._
  implicit class StandardStringToKey(name: String) extends RichString(name)

  val clipPath = AttrKey(name = "clip-art")
  val cx = "cx".attr
  val cy = "cx".attr
  val d = "d".attr
  val dx = "dx".attr
  val dy = "dy".attr
  val fill = "fill".attr
  val fillOpacity = "fill-opacity".attr
  val fontFamily = "font-family".attr
  val fontSize = "font-size".attr
  val fx = "fx".attr
  val fy = "fy".attr
  val gradientTransform = "gradientTransform".attr
  val gradientUnits = "gradientUnits".attr
  val markerEnd = "marker-end".attr
  val markerMid = "marker-mid".attr
  val markerStart = "marker-start".attr
  val offset = "offset".attr
  val opacity = "opacity".attr
  val patternContentUnits = "patternContentUnits".attr
  val patternUnits = "patternUnits".attr
  val points = "points".attr
  val preserveAspectRatio = "preserveAspectRatio".attr
  val r = "r".attr
  val rx = "rx".attr
  val ry = "ry".attr
  val spreadMethod = "spreadMethod".attr
  val stopColor = "stop-color".attr
  val stopOpacity = "stroke-opacity".attr
  val stroke = "stroke".attr
  val strokeDasharray = "stroke-dasharray".attr
  val strokeLinecap = "stoke-linecap".attr
  val strokeOpacity = "stroke-opacity".attr
  val strokeWidth = "stroke-width".attr
  val textAnchor = "text-anchor".attr
  val transform = "transform".attr
  val version = "version".attr
  val viewBox = "view-box".attr
  val x1 = "x1".attr
  val x2 = "x2".attr
  val x = "x".attr
  val xlinkActuate = AttrKey("xlink:actuate", Some(XLINK))
  val xlinkArcrole = AttrKey("xlink:arcrole", Some(XLINK))
  val xlinkHref = AttrKey("xlink:href", Some(XLINK))
  val xlinkRole = AttrKey("xlink:role", Some(XLINK))
  val xlinkShow = AttrKey("xlink:show", Some(XLINK))
  val xlinkTitle = AttrKey("xlink:title", Some(XLINK))
  val xlinkType = AttrKey("xlink:type", Some(XLINK))
  val xmlBase = AttrKey("xml:base", Some(XML))
  val xmlLang = AttrKey("xml:lang", Some(XML))
  val xmlSpace = AttrKey("xml:space", Some(XML))
  val y1 = "y1".attr
  val y2 = "y2".attr
  val y = "y".attr
}

object SVGAttributes extends SVGAttributes

/**
 * Style attributes. These attributes are set on the style property
 * of an element. Some of them duplicate attributes found on other
 * elements e.g. height and width.
 */
trait Style {
  implicit class StyleToKey(name: String) extends RichString(name)
  val border = "border".style
  val fill = "fill".style
  val height = "height".style
  val heightS = height
  val lineHeight = "lineHeight".style
  val stroke = "stroke".style
  val textAlign = "textAlign".style
  val width = "width".style
  val widthS = width
}

object Style extends Style

trait UIEvents {
  implicit class FunctionToKey(eventType: String) extends RichString(eventType)
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

