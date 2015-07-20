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

/**
 * It's not clear why I can't just build this into the VNode object
 * and let subclasses call the superclass method versus having this
 * function stuck out here in the middle of nowhere.
 */
private[vdom] trait DiffModule {
  /**
   * Diff two VNode's and return a patch that makes original look like target.
   * This function determines if the original and target are so different
   * that it has to be replaced by a new node (e.g. insert, remove or replace)
   * versus an update. If they are similar, they are diffed against
   * each other.
   *
   * @param original the current VNode description
   * @param target the target VNode description
   * @return a Patch that would make original look like target
   */
  def diff(original: VNode, target: VNode): Patch = {
    if (original == target) EmptyPatch
    else if (original == VNode.empty) InsertPatch(target)
    else if (target == VNode.empty) RemovePatch()
    else (original, target) match {
      case (o: VirtualText, t: VirtualText) => o.diff(t)
      case (o: VirtualElementNode, t: VirtualElementNode) => o.diff(t)
      case (o: ThunkNode, t: ThunkNode) => o.diff(t)
      case (o: ThunkNode, _) => diff(o.f(), target)
      case (_, t) => ReplacePatch(target)
    }
  } 
}

private[vdom] object DiffModule extends DiffModule