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
   * Keys are really builders of `KeyValue` objects.
   */
  trait KeyPart { self =>
    def name: String
    def namespace: Option[String] = None
  }

  /**
   * Create KeyParts easily given a string.
   */
  class RichString(val name: String) {
    def attr = AttrKey(name)
    def style = StyleKey(name)
    def func = FunctionKey(name)
  }

  /**
   * Attribute that is part of an element.
   */
  case class AttrKey(val name: String, override val namespace: Option[String] = None) extends KeyPart { self =>
    def :=[T](v: Option[T]): KeyValue[T] = KeyValue[T](self, v)
    def :=[T](v: T): KeyValue[T] = :=[T](Some(v))
  }

  /**
   * Property that is part of a style.
   */
  case class StyleKey(val name: String) extends KeyPart { self =>
    def :=[T](v: Option[T]): KeyValue[T] = KeyValue[T](self, v)
    def :=[T](v: T): KeyValue[T] = :=[T](Some(v))
  }

  /**
   * KeyValues that hold functions actually hold a tuple of information
   * relevant to calling a handler when a dom.Event is fired.
   */
  type FunctionArgs = (events.Handler, events.Matcher, Option[Boolean])

  /**
   * Value that is a function for an event handler. The entire
   * event handling capabilities need to be lifted to a string
   * based action framework to allow server side rendering to 
   * work better.
   */
  case class FunctionKey(val name: String) extends KeyPart { self =>
    def ~~>(v: events.Handler) = KeyValue[FunctionArgs](self, Some((v, events.Matcher.MatchRoot, None)))
    def ~~>(matcher: events.Matcher, v: events.Handler, useCapture: Option[Boolean] = None) =
      KeyValue[FunctionArgs](self, Some((v, matcher, useCapture)))

  }

  /**
   * Combination of keys and values. A value of None should indicate
   * that something should be unset or removed. What unset or remove
   * means is Backend dependent.
   */
  case class KeyValue[T](val key: KeyPart, val value: Option[T]) {
    /**
     * Convenience function to unset or remove a value.
     */
    def unset = copy(value = None)
  }

  implicit class OptionOps[T](lhs: Option[T]) {
    def fuzzyEq(rhs: Option[T]) = (lhs, rhs) match {
      case (None, None) => true
      case (Some(l), Some(r)) => l == r
      case _ => false
    }

    /**
     * rhs = None acts like a wildcard and matches anything
     * on lhs. But lhs = None only matches a rhs None.
     */
    def wildcardEq(rhs: Option[T]) = (rhs, lhs) match {
      case (None, None) => true
      case (Some(l), Some(r)) => l == r
      case (None, _) => false
      case (_, None) => true
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
