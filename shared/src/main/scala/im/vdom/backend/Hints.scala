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

import scala.collection.immutable.BitSet
import scala.language._

/**
 * Basic hint structure. Hints are encoded as bit flags..Sub-classes
 * are made for different hint usage types so that they can
 * be customized with additional usage specific hint information.
 * This is essentially like React's DOMProperty hint scaffolding.
 */
trait Hints {
  /** Hint information for working with values */
  def values: BitSet
}

/**
 * Hints for working with Elements
 */
case class AttrHint(
  val values: BitSet) extends Hints

/** Hints for Styles. */
case class StyleHint(
  val values: BitSet) extends Hints

/**
 * Hints for Elements.
 */
case class ElHint(
  val values: BitSet) extends Hints

/**
 *  Some of these hints are only relevant with certain backends,
 *  such as the DOM backend e.g. MustUseProperty. Some, are
 *  related to multiple backends, such as HasPositiveNumericValue.
 *
 *  Hints can be combined through '+'.
 */
object Hints {

  /** Convert single BitSet to an AttrHint */
  implicit def hintToAttrHint(hints: BitSet) = AttrHint(values = hints)

  /** Convert multiple BitSets to an AttrHint */
  implicit def hintsToAttrHints(hints: BitSet*) =
    AttrHint(values = hints.fold(EmptyHints)(_ & _))

  /** Empty hint set. */
  val EmptyHints = BitSet.empty

  /** Convenience converter. */
  implicit def toBitSet(i: Int) = BitSet(i)

  /** Should use set/get/remove*Property */
  val MustUseAttribute = 1

  /** Must set value NOt using the attribute API */
  val MustUseProperty = 2

  /** Setting the value has side effects. */
  val HasSideEffects = 3

  /** Whether property should be removed when set to a falsy value. */
  val HasBooleanValue = 4

  /** Not sure what this means in react. */
  val HasOverloadedBooleanValue = 5

  /** Whether property must be numeric or parse as numeric and should be removed when set to a falsy value */
  val HasNumericValue = 6

  /** Numeric and positive. */
  val HasPositiveNumericValue = 7

  // Element hints are separately enumerated from attribute hints.
  val OmitClosingTag = 1
  val NewlineEating = 2

  // Style hints
  val Unitless = 1

  /**
   * Empty hints with empty value hints.
   */
  val EmptyAttrHints = AttrHint(values = EmptyHints)
}

trait DOMStyleHints {
  import Hints._

  private val styleHints: Map[String, StyleHint] = Map(
    "animationIterationCount" -> StyleHint(values = Unitless),
    "boxFlex" -> StyleHint(values = Unitless),
    "boxFlexGroup" -> StyleHint(values = Unitless),
    "boxOrdinalGroup" -> StyleHint(values = Unitless),
    "columnCount" -> StyleHint(values = Unitless),
    "flex" -> StyleHint(values = Unitless),
    "flexGrow" -> StyleHint(values = Unitless),
    "flexPositive" -> StyleHint(values = Unitless),
    "flexShrink" -> StyleHint(values = Unitless),
    "flexNegative" -> StyleHint(values = Unitless),
    "flexOrder" -> StyleHint(values = Unitless),
    "gridRow" -> StyleHint(values = Unitless),
    "gridColumn" -> StyleHint(values = Unitless),
    "fontWeight" -> StyleHint(values = Unitless),
    "lineClamp" -> StyleHint(values = Unitless),
    "lineHeight" -> StyleHint(values = Unitless),
    "opacity" -> StyleHint(values = Unitless),
    "order" -> StyleHint(values = Unitless),
    "orphans" -> StyleHint(values = Unitless),
    "tabSize" -> StyleHint(values = Unitless),
    "widows" -> StyleHint(values = Unitless),
    "zIndex" -> StyleHint(values = Unitless),
    "zoom" -> StyleHint(values = Unitless),

    // SVG-related properties
    "fillOpacity" -> StyleHint(values = Unitless),
    "stopOpacity" -> StyleHint(values = Unitless),
    "strokeDashoffset" -> StyleHint(values = Unitless),
    "strokeOpacity" -> StyleHint(values = Unitless),
    "strokeWidth" -> StyleHint(values = Unitless))

  def styleHint(name: String) = styleHints.get(name)
}
protected[backend] object DOMStyleHints extends DOMStyleHints

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
  def elHint(name: String) = elHints.get(name)
}

protected[backend] object DOMElHints extends DOMElHints

trait DOMAttrHints {
  import Hints._
  import Constants.NS._

  private implicit def toAttrHint(i: Int) = AttrHint(values = BitSet(i))
  
  private val attrHints: Map[String, AttrHint] = Map(
    "allowFullScreen" -> AttrHint(values = BitSet(HasBooleanValue) | BitSet(MustUseAttribute)),
    "async" -> AttrHint(values = HasBooleanValue),
    "autoPlay" -> AttrHint(values = HasBooleanValue),
    "checked" -> AttrHint(values = BitSet(MustUseProperty) | BitSet(HasBooleanValue)),
    "class" -> AttrHint(values = MustUseAttribute),
    "disabled" -> AttrHint(values = BitSet(MustUseAttribute) | BitSet(HasBooleanValue)),
    "download" -> AttrHint(values = HasOverloadedBooleanValue),
    "formNoValidate" -> AttrHint(values = HasBooleanValue),
    "hidden" -> AttrHint(values = BitSet(MustUseAttribute) | BitSet(HasBooleanValue)),
    "height" -> AttrHint(MustUseAttribute),
    "hidden" -> AttrHint(MustUseAttribute),
    "id" -> AttrHint(MustUseProperty),
    "readOnly" -> AttrHint(values = BitSet(MustUseProperty) | BitSet(HasBooleanValue)),
    "required" -> AttrHint(values = HasBooleanValue),
    "reversed" -> AttrHint(values = HasBooleanValue),
    "scoped" -> AttrHint(values = HasBooleanValue),
    "seamless" -> AttrHint(values = BitSet(MustUseAttribute) | BitSet(HasBooleanValue)),
    "selected" -> AttrHint(values = BitSet(MustUseProperty) | BitSet(HasBooleanValue)),
    "value" -> AttrHint(BitSet(MustUseProperty) | BitSet(HasSideEffects)),
    "width" -> AttrHint(MustUseAttribute))
    
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
  def attrHint(name: String) = attrHints.get(name) orElse svgHints.get(name)

}

protected[backend] object DOMAttrHints extends DOMAttrHints

/** All DOM related hints. */
trait DOMHints extends DOMStyleHints with DOMElHints with DOMAttrHints

 