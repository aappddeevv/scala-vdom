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

package object vdom {

  /**
   * A marker trait for an object that performs a side-effect. There could
   * be substantial machinery underneath to perfrom the action.
   */
  trait Performer[T] extends (T => Unit)

  /**
   * An action to be applied to elements. The programmer task is to create
   * action objects that have enough "configuration" data to properly execute
   * a side-effect function on a DOM Element. Attributes and properties
   * are handled this way because of the complexity of information needed
   * to set them correctly.
   */
  trait ElementAction extends Performer[dom.Element] {
    def key: AttributeKey
  }

  /** List of attribute actions. */
  type Attrs = Seq[ElementAction]

  /**
   * List of empty attribute actions.
   */
  val EmptyAttrs = Seq[ElementAction]()

  /**
   * An attribute key has to have, at the very least, a name.
   */
  trait AttributeKey {
    /** Name for external use */
    def name: String

    /**
     * Create an `AttrAction` for an optional value. None should
     * imply unsetting the value or no value to set. Semantics
     * are left to subclasses.
     */
    def :=[T](v: Option[T]): ElementAction
  }

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
   * A wrapper around a string that build `ElementActon`s once you use the builder
   * function `:=`.
   *
   * Unlike scalatags, we do not have an "action" object on each value set. We only
   * need to worry about actions based on the key type, not key+value.
   *
   * @param name Name of key. Use the official HTML name for the name.
   * @param hints Bitset values hints that can be used in ElementActions that are key specific.
   */
  case class Key(name: String, hints: ElementHints = EmptyElementHints) extends AttributeKey {
    def :=[T](v: Option[T]) = AttrAction(this, v)
    def :=[T](v: T) = AttrAction(this, Some(v))
  }

  /** Name and value pair. Use this to perform the side effect on a DOM Element. */
  case class AttrAction[T](val key: Key, val value: Option[T]) extends ElementAction {
    def apply(el: dom.Element): Unit = {
      //println(s"applied AttrAction: $key, $value")
      value.fold(el.removeAttribute(key.name)) { v =>
        //println(s"applying: ${key.name} -> ${value} to $el")
        if (!(key.hints.values & MustUseAttribute).isEmpty)
          el.setAttribute(key.name, v.toString)
        else
          el.asInstanceOf[js.Dynamic].updateDynamic(key.name)(v.asInstanceOf[js.Any])
      }
    }
    override def toString(): String = {
      "AttrAction[key=" + key + ", value=" + value + "]"
    }
  }

  /**
   * Style key.
   */
  case class StyleKey(name: String) extends AttributeKey {
    def :=[T](v: Option[T]) = StyleAction(this, v)
    def :=[T](v: T) = StyleAction(this, Some(v))
  }

  /**
   * Style action. Directly adds value to the "style" property of the Element
   */
  case class StyleAction[T](val key: StyleKey, value: Option[T]) extends ElementAction {
    def apply(el: dom.Element): Unit = {
      val style = el.asInstanceOf[dom.html.Element].style
      value.map(_.toString).fold[Unit](style.removeProperty(key.name))(v => style.setProperty(key.name, v, ""))
    }
  }

  /**
   * A key which produces `ByTheBookAttrAction`s.
   */
  case class ByTheBookKey(name: String) extends AttributeKey {
    def :=[T](v: Option[T]) = ByTheBookAttrAction(this, v)
    def :=[T](v: T) = ByTheBookAttrAction(this, Some(v))
  }

  /**
   * No hints processing at all. Simple sets the attribute as a string.
   */
  case class ByTheBookAttrAction[T](val key: ByTheBookKey, val value: Option[T]) extends ElementAction {
    def apply(el: dom.Element): Unit =
      value.fold(el.removeAttribute(key.name))(v => el.setAttribute(key.name, v.toString))
  }

  /** Enable "aname".attr syntax */
  class RichElementActionKey(val name: String) {
    def attr = Key(name)
    def attr(hints: ElementHints) = Key(name, hints)
  }

  /** Enable "aname".attr syntax but produce `ByTheBookKey`s */
  class RichElementByTheBookActionKey(val name: String) {
    def attr = ByTheBookKey(name)
  }

  class RichElementStyleActionKey(val name: String) {
    def attr = StyleKey(name)
    def sattr = attr
  }

  trait StandardHTML5Attributes {

    implicit class StandardStringToKey(name: String) extends RichElementActionKey(name)

    val acceptCharSet = "accept-charset".attr

    val checked = "checked".attr(MustUseProperty)

    /** Class attribute */
    val `class` = "class".attr(MustUseAttribute)

    /** Easier to type class attribute */
    val cls = `class`

    var contentEditable = "contenteditable".attr

    /** Right click context menu */
    val contextMenu = "contextmenu".attr

    val dir = "dir".attr

    /** Use this to hide an element instead of using class tricks. */
    val hidden = "hidden".attr(MustUseAttribute)

    val href = "href".attr

    val htmlFor = "for".attr

    /** id attribute */
    val id = "id".attr(MustUseProperty)

    val lang = "lang".attr

    val selected = "selected".attr(MustUseProperty)

    /** Inline style */
    val style = "style".attr

    /** Tab order */
    val tabIndex = "tabindex".attr

    /** Title */
    val title = "title".attr

    /** The value, typically of an input control. */
    val value = "value".attr(MustUseProperty & HasSideEffects)
  }

  object StandardHTML5Attributes extends StandardHTML5Attributes

  trait CustomHTML5Attributes {

    implicit class CustomStringToKey(name: String) extends RichElementByTheBookActionKey(name)

    /** Custom data attribute */
    def data(name: String) = Key("data-" + name)

    /** For assistive technologies. */
    def aria(name: String) = Key("aria-" + name)
  }

  object CustomHTML5Attributes extends CustomHTML5Attributes

  /**
   * Standard HTML5 attributes. Usually, you'll import this.
   */
  object HTML5Attributes extends StandardHTML5Attributes with CustomHTML5Attributes

  trait Style {
    implicit class StyleToKey(name: String) extends RichElementStyleActionKey(name)

    val textAlign = "textAlign".attr
    val lineHeight = "lineHeight".attr
    val border = "border".attr
    val height = "height".attr
    val width = "width".attr
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

}
