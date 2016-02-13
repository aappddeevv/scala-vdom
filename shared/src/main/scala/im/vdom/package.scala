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

import scala.language._

package object vdom {

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
   * Generate a Patch that describes the differences between original and target.
   */
  def diff(original: VNode, target: VNode): Patch = Diff.diff(original, target)

  /**
   * Helpers for comparing Option values.
   */
  implicit class OptionOps[T](lhs: Option[T]) {
    
    /**
     * rhs = None acts like a wildcard and matches anything
     * on lhs. But lhs = None only matches a rhs None.
     */
    def wildcardEq(rhs: Option[T]) = (lhs, rhs) match {
      case (None, None) => true
      case (None, _) => false
      case (_, None) => true
      case (Some(l), Some(r)) => l == r
    }

    /**
     * Negated `===`
     */
    def /==(rhs: Option[T]) = !(===(rhs))
    /**
     * Equal only if they are both defined and the defined values are equal.
     */
    def ===(rhs: Option[T]) = lhs.toRight(false) == rhs.toRight(true)
  }

  /**
   * Enable explicit `.toPatch` notation on a sequence of patches.
   */
  implicit class ToPatch(seq: Seq[Patch]) {
    def to1Patch = seqPatchToPatch(seq)
  }

}
