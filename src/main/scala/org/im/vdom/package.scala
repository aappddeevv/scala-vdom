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

import _root_.org.scalajs.dom
import scalajs.js
import language._
import scala.annotation.implicitNotFound
import collection.immutable.BitSet
import scalajs.js.UndefOr

/**
 * TODO: Decouple the DOM implementation part from the expression of
 * the attributes and push the implementation part into the `Backend`.
 */
package object vdom {

  /**
   * Basic hint structure for working with elements.
   */
  trait Hints {
    /** Hint information for working with values */
    def values: BitSet
  }

  /**
   * EVerything below here relates to setting attributes on elements so is a more
   * specific concept of an `ElementAction` and `AttributeKey`. We can use
   * scalatags to help create Performer objects once we become a more mature
   * library. Ask Li for some help.
   */

  /**
   * Hints for working with elements and their properties/attributes. This
   * is essentially like React's DOMProperty injection concept.
   */
  case class ElementHints(val values: BitSet) extends Hints

  /** Empty hint set. */
  val EmptyHints = BitSet.empty

  /** Should use set/get/remove*Property */
  val MustUseAttribute = BitSet(1)

  /** Must set value NOt using the attribute API */
  val MustUseProperty = BitSet(2)

  /** Setting the value has side effects. */
  val HasSideEffects = BitSet(4)

  /**
   * Empty element hints with empty value hints.
   */
  val EmptyElementHints = ElementHints(values = EmptyHints)

  /**
   * Pure convenience function for now to promote BitSet to ElementHints
   * If you specify more than just a BitSet for your hints, you need to use
   * the full object syntax.
   */
  implicit def hintsToElementHints(hints: BitSet) = ElementHints(values = hints)

  /**
   * Keys are really builders of `KeyValue` objects.
   */
  trait KeyPart { self =>
    def name: String
    def :=[T](v: Option[T]): KeyValue[T] = KeyValue[T](self, v)
    def :=[T](v: T): KeyValue[T] = :=[T](Some(v))
  }

  case class AttrKey(val name: String) extends KeyPart
  case class StyleKey(val name: String) extends KeyPart

  /**
   * Combination of keys and values. A value of None should indicate
   * that something should be unset or removed.
   */
  case class KeyValue[T](val key: KeyPart, val value: Option[T]) {
    /**
     * Convenience function to unset a value.
     */
    def unset = copy(value = None)
  }

  /**
   * Allow `.attr` on strings to create a `KeyValue`.
   */
  class RichAttrKey(name: String) {
    def attr = AttrKey(name)
  }

  /**
   * Allow `.style` on strings to create a `KeyValue`.
   */
  class RichStyleKey(name: String) {
    def style = StyleKey(name)
  }

  //
  //  /** Name and value pair. Use this to perform the side effect on a DOM Element. */
  //  case class AttrAction[T](val key: Key, val value: Option[T]) extends ElementAction {
  //    def apply(el: dom.Element): Unit = {
  //      //println(s"applied AttrAction: $key, $value")
  //      value.fold(el.removeAttribute(key.name)) { v =>
  //        //println(s"applying: ${key.name} -> ${value} to $el")
  //        if (!(key.hints.values & MustUseAttribute).isEmpty)
  //          el.setAttribute(key.name, v.toString)
  //        else
  //          el.asInstanceOf[js.Dynamic].updateDynamic(key.name)(v.asInstanceOf[js.Any])
  //      }
  //    }
  //    override def toString(): String = {
  //      "AttrAction[key=" + key + ", value=" + value + "]"
  //    }
  //  }

  trait StandardHTML5Attributes {

    implicit class StandardStringToKey(name: String) extends RichAttrKey(name)

    val acceptCharSet = "accept-charset".attr

    val checked = "checked".attr

    /** Class attribute */
    val `class` = "class".attr

    /** Easier to type class attribute */
    val cls = `class`

    var contentEditable = "contenteditable".attr

    /** Right click context menu */
    val contextMenu = "contextmenu".attr

    val dir = "dir".attr

    /** Use this to hide an element instead of using class tricks. */
    val hidden = "hidden".attr

    val href = "href".attr

    val htmlFor = "for".attr

    /** id attribute */
    val id = "id".attr

    val lang = "lang".attr

    val selected = "selected".attr

    /** Inline style */
    val style = "style".attr

    /** Tab order */
    val tabIndex = "tabindex".attr

    /** Title */
    val title = "title".attr

    /** The value, typically of an input control. */
    val value = "value".attr
  }

  object StandardHTML5Attributes extends StandardHTML5Attributes

  trait CustomHTML5Attributes {
    implicit class CustomStringToKey(name: String) extends RichAttrKey(name)

    /** Custom data attribute */
    def data(name: String) = AttrKey("data-" + name)

    /** For assistive technologies. */
    def aria(name: String) = AttrKey("aria-" + name)
  }

  object CustomHTML5Attributes extends CustomHTML5Attributes

  /**
   * Standard HTML5 attributes. Usually, you'll import this.
   */
  object HTML5Attributes extends StandardHTML5Attributes with CustomHTML5Attributes

  trait Style {
    implicit class StyleToKey(name: String) extends RichStyleKey(name)

    val textAlign = "textAlign".style
    val lineHeight = "lineHeight".style
    val border = "border".style
    val height = "height".style
    val width = "width".style
  }

  object Style extends Style

  implicit class OptionOps[T](lhs: Option[T]) {
    def fuzzyEq(rhs: Option[T]) = (rhs, lhs) match {
      case (None, None) => true
      case (Some(l), Some(r)) => l == r
      case _ => false
    }

    def /==(rhs: Option[T]) = !(===(rhs))
    def ===(rhs: Option[T]) = lhs.toRight(false) == rhs.toRight(true)
  }

  /**
   * Virtual nodes may have keys to improve diff'ing performance.
   */
  type VNodeKey = String

  /**
   * Convenience methods for DOM Nodes and Elements.
   */
  implicit class RichNode[T <: org.scalajs.dom.Node](n: T) {
    import UndefOr._
    def parentOpt: UndefOr[org.scalajs.dom.Node] = {
      val p = n.parentNode
      if (p == null) js.undefined
      p
    }
  }

  /**
   * Reduction to a single patch object using composition. All
   * patches will apply to the same input object when applied.
   */
  implicit def seqPatchToPatch(seq: Seq[Patch]): Patch = seq.fold(EmptyPatch)((p, n) => p andThen n)

  /**
   * Enable explicit `.toPatch` notation on a sequence of patches.
   */
  implicit class ToPatch(seq: Seq[Patch]) {
    def toPatch = seqPatchToPatch(seq)
  }

  /**
   * Generate a Patch that describes the differences between original and target.
   */
  def diff(original: VNode, target: VNode): Patch = DiffModule.diff(original, target)
  
}
