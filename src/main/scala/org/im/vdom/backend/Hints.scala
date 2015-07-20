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
package backend

import scalajs.js
import js._
import org.scalajs.dom
import scala.scalajs.js.UndefOr
import UndefOr._
import scala.collection.immutable.BitSet

/**
 * Basic hint structure for working with elements.
 */
trait Hints {
  /** Hint information for working with values */
  def values: BitSet
}

/**
 * Hints for working with elements and their properties/attributes. This
 * is essentially like React's DOMProperty injection concept.
 */
case class AttrHint(
  /** Hints in bit form. */
  val values: BitSet) extends Hints

object Hints {
  /**
   * Pure convenience function for now to promote BitSet to ElementHints
   * If you specify more than just a BitSet for your hints, you need to use
   * the full object syntax.
   */
  implicit def hintsToAttrHint(hints: BitSet) = AttrHint(values = hints)

  /** Empty hint set. */
  val EmptyHints = BitSet.empty

  /** Should use set/get/remove*Property */
  val MustUseAttribute = BitSet(1)

  /** Must set value NOt using the attribute API */
  val MustUseProperty = BitSet(2)

  /** Setting the value has side effects. */
  val HasSideEffects = BitSet(4)

  /**
   * Empty hints with empty value hints.
   */
  val EmptyAttrHints = AttrHint(values = EmptyHints)
}

trait AttrHints {
  import Hints._
  import Constants.NS._

  private val attrHints: Map[String, AttrHint] = Map(
    "checked" -> MustUseProperty,
    "class" -> MustUseAttribute,
    "height" -> MustUseAttribute,
    "hidden" -> MustUseAttribute,
    "id" -> MustUseProperty,
    "selected" -> MustUseProperty,
    "value" -> (MustUseProperty & HasSideEffects),
    "width" -> MustUseAttribute)

  private val svgHints: Map[String, AttrHint] = Map(
    "clipPath" -> AttrHint(MustUseAttribute),
    "cx" -> MustUseAttribute,
    "cy" -> MustUseAttribute,
    "d" -> MustUseAttribute,
    "dx" -> MustUseAttribute,
    "dy" -> MustUseAttribute,
    "fill" -> MustUseAttribute,
    "fillOpacity" -> AttrHint(MustUseAttribute),
    "fontFamily" -> AttrHint(MustUseAttribute),
    "fontSize" -> AttrHint(MustUseAttribute),
    "fx" -> MustUseAttribute,
    "fy" -> MustUseAttribute,
    "gradientTransform" -> AttrHint(MustUseAttribute),
    "gradientUnits" -> AttrHint(MustUseAttribute),
    "markerEnd" -> AttrHint(MustUseAttribute),
    "markerMid" -> AttrHint(MustUseAttribute),
    "markerStart" -> AttrHint(MustUseAttribute),
    "offset" -> MustUseAttribute,
    "opacity" -> MustUseAttribute,
    "patternContentUnits" -> MustUseAttribute,
    "patternUnits" -> MustUseAttribute,
    "points" -> MustUseAttribute,
    "preserveAspectRatio" -> MustUseAttribute,
    "r" -> MustUseAttribute,
    "rx" -> MustUseAttribute,
    "ry" -> MustUseAttribute,
    "spreadMethod" -> MustUseAttribute,
    "stopColor" -> AttrHint(MustUseAttribute),
    "stopOpacity" -> AttrHint(MustUseAttribute),
    "stroke" -> MustUseAttribute,
    "strokeDasharray" -> AttrHint(MustUseAttribute),
    "strokeLinecap" -> AttrHint(MustUseAttribute),
    "strokeOpacity" -> AttrHint(MustUseAttribute),
    "strokeWidth" -> AttrHint(MustUseAttribute),
    "textAnchor" -> AttrHint(MustUseAttribute),
    "transform" -> MustUseAttribute,
    "version" -> MustUseAttribute,
    "viewBox" -> AttrHint(MustUseAttribute),
    "x1" -> MustUseAttribute,
    "x2" -> MustUseAttribute,
    "x" -> MustUseAttribute,
    "xlinkActuate" -> AttrHint(MustUseAttribute),
    "xlinkArcrole" -> AttrHint(MustUseAttribute),
    "xlinkHref" -> AttrHint(MustUseAttribute),
    "xlinkRole" -> AttrHint(MustUseAttribute),
    "xlinkShow" -> AttrHint(MustUseAttribute),
    "xlinkTitle" -> AttrHint(MustUseAttribute),
    "xlinkType" -> AttrHint(MustUseAttribute),
    "xmlBase" -> AttrHint(MustUseAttribute),
    "xmlLang" -> AttrHint(MustUseAttribute),
    "xmlSpace" -> AttrHint(MustUseAttribute),
    "y1" -> MustUseAttribute,
    "y2" -> MustUseAttribute,
    "y" -> MustUseAttribute)

  /** Get a hint or None. */
  def hint(name: String) = attrHints.get(name) orElse svgHints.get(name)

}

protected[backend] object AttrHints extends AttrHints
