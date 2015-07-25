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

import scala.language._

import org.im.vdom.DiffModule
import org.im.vdom.EmptyPatch
import org.im.vdom.Patch
import org.im.vdom.VNode

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

  /**
   * The exception type in this system.
   */
  class VDomException(msg: String, parent: Throwable = null) extends RuntimeException(msg, parent)

}
