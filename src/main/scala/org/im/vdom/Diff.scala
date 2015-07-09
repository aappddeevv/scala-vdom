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

trait DiffModule {

  /**
   * Diff two VNode's and return a patch that makes original look like target.
   *
   * Since VNode is a sealed trait, we can match out on types and ensure that
   * diffing is performed on like objects and optimize diff detection when its
   * not.
   *
   * @param original the current VNode description
   * @param target the target
   * @return a Patch that would make original look like target
   */
  def diff(original: VNode, target: VNode): Patch = {
    //println(s"diffing: $original, $target")
    if (original == target) EmptyPatch
    else if (original == VNode.empty) InsertPatch(target)
    else if (target == VNode.empty) RemovePatch()
    else (original, target) match {
      case (o: VirtualText, t: VirtualText) => o.diff(t)
      case (o: VirtualElementNode, t: VirtualElementNode) => o.diff(t)
      case (_, t) => ReplacePatch(target)
    }
  }

  /**
   * Reduction to a single patch object using composition. All
   * patches will apply to the same DOM Element when applied.
   */
  implicit def seqPatchToPatch(seq: Seq[Patch]): Patch = seq.fold(EmptyPatch)((p, n) => p andThen n)

  /**
   * Enable explicit `.toPatch` notation on a sequence of patches.
   */
  implicit class ToPatch(seq: Seq[Patch]) {
    def toPatch = seqPatchToPatch(seq)
  }
 
}

object DiffModule extends DiffModule