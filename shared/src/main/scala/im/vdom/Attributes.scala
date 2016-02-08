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

  val accept = "accept".attr
  val acceptCharSet = "accept-charset".attr
  val accessKey = "accessKey".attr
  val action = "action".attr
  val allowFullScreen = "allowFullScreen".attr
  val allowTransparency = "allowTransparency".attr
  val alt = "alt".attr
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
  val scope = "scope".attr
  val scoped = "scoped".attr
  val scrolling = "scrolling".attr
  val seamless = "seamless".attr
  val selected = "selected".attr
  val shape = "shape".attr
  val size = "size".attr
  val sizes = "sizes".attr
  val span = "span".attr
  val spellCheck = "spellCheck".attr
  val src = "src".attr
  val srcDoc = "srcDoc".attr
  val style = "style".attr
  val tabIndex = "tabindex".attr
  val title = "title".attr
  val value = "value".attr
  val widthA = "width".attr // conflicts with style
  val heightA = "height".attr // conflicts with style

  // Don't forget these...
  /**
   * async autoComplete autoFocus autoPlay 
   * 
   * cellPadding cellSpacing charSet checked
   * classID className colSpan cols content contentEditable contextMenu controls
   * coords crossOrigin 
   * data dateTime defer dir disabled download draggable 
   * encType
   * form formAction formEncType formMethod formNoValidate formTarget frameBorder
   * 
   * headers height hidden high href hrefLang htmlFor httpEquiv 
   * icon id 
   * label lang list loop low 
   * manifest marginHeight marginWidth max maxLength media mediaGroup
   * method min multiple muted 
   * 
   * name noValidate 
   * open optimum 
   * pattern placeholder poster preload 
   * radioGroup readOnly rel required role rowSpan rows sandbox 
   * 
   * srcSet start step style 
   * tabIndex target title type 
   * useMap 
   * value 
   * width wmode
   */
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
trait Styles {
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

object Styles extends Styles
