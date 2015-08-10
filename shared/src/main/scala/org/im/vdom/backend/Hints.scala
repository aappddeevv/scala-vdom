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

import scala.collection.immutable.BitSet

/**
 * Basic hint structure for working with an element.
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

/**
 * Hints for Elements.
 */
case class ElHint(
  /** Hints in bit form. */
  val values: BitSet) extends Hints

/**
 *  Some of these hints are only relevant with certain backends,
 *  such as the DOM backend e.g. MustUseProperty. Some, are
 *  related to multiple backends, such as HasPositiveNumericValue.
 */
object Hints {

  /** Convert single BitSet to an AttrHint */
  implicit def hintToAttrHint(hints: BitSet) = AttrHint(values = hints)

  /** Convert multiple BitSets to an AttrHint */
  implicit def hintsToAttrHints(hints: BitSet*) =
    AttrHint(values = hints.fold(EmptyHints)(_ & _))

  /** Empty hint set. */
  val EmptyHints = BitSet.empty

  /** Should use set/get/remove*Property */
  val MustUseAttribute = BitSet(1)

  /** Must set value NOt using the attribute API */
  val MustUseProperty = BitSet(1 << 2)

  /** Setting the value has side effects. */
  val HasSideEffects = BitSet(1 << 3)

  val HasBooleanValue = BitSet(1 << 4)
  val HasNumericValue = BitSet(1 << 5)
  val HasPositiveNumericValue = BitSet(1 << 6)
  val HasOverloadedBooleanValue = BitSet(1 << 8)

  // Element hints are separately enumerated from attribute hints.
  val OmitClosingTag = BitSet(1)
  val NewlineEating = BitSet(2)

  /**
   * Empty hints with empty value hints.
   */
  val EmptyAttrHints = AttrHint(values = EmptyHints)
}

trait DOMElHints {
  import Hints._

  private val elHints: Map[String, ElHint] = Map(
    "area" -> ElHint(values = OmitClosingTag),
    "base" -> ElHint(values = OmitClosingTag),
    "br" -> ElHint(values = OmitClosingTag),
    "col" -> ElHint(values = OmitClosingTag),
    "hr" -> ElHint(values = OmitClosingTag),
    "img" -> ElHint(values = OmitClosingTag),
    "input" -> ElHint(values = OmitClosingTag),
    "keygen" -> ElHint(values = OmitClosingTag),
    "listing" -> ElHint(values = NewlineEating),
    "link" -> ElHint(values = OmitClosingTag),
    "meta" -> ElHint(values = OmitClosingTag),
    "param" -> ElHint(values = OmitClosingTag),
    "pre" -> ElHint(values = NewlineEating),
    "source" -> ElHint(values = OmitClosingTag),
    "textarea" -> ElHint(values = NewlineEating),
    "track" -> ElHint(values = OmitClosingTag),
    "wbr" -> ElHint(values = OmitClosingTag))

  /** Get a hint or None. */
  def hint(name: String) = elHints.get(name)
}

private[backend] object DOMElHints extends DOMElHints

trait DOMAttrHints {
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

private[backend] object DOMAttrHints extends DOMAttrHints
