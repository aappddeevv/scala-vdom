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

import scala.concurrent.{ Promise, ExecutionContext, Future }
import scala.util.{ Try, Success, Failure }
import scala.util.control.NonFatal

/**
 * A Patch holds "difference" information that can be applied
 * to another object (the target) to make it look like another
 * (the source) object. The "difference" information must
 * be interpreted for each backend environment.
 * 
 * You can create patches either from diffing two virtual DOM
 * trees or you can just manually create the patches and compose
 * them using a sequence or `andThen` essentially creating a
 * patch stream or template language of DOM "updates."
 * 
 * It is quite passible that another layer of abstraction creates
 * patches directly as an intermediate "language."
 */
sealed trait Patch {
  /**
   * Route this path to another part of the tree when its run.
   */
  def applyTo(path: Seq[Int] = Nil) = PathPatch(this, path)

  /**
   * Route this path to a specific child index when it is run.
   */
  def applyTo(path: Int) = PathPatch(this, Seq(path))
  
  /**
   * Sequence a patch before another.
   */
  def andThen(right: Patch) = AndThenPatch(this, right)
}

case object EmptyPatch extends Patch
case class PathPatch(patch: Patch, path: Seq[Int] = Nil) extends Patch
case object RemovePatch extends Patch

// Can I refactor these to simpler patches? e.g replace = create + remove + append?
case class ReplacePatch(replacement: VNode) extends Patch
case class InsertPatch(vnode: VNode) extends Patch

case class TextPatch(content: String) extends Patch
case class KeyValuePatch(elActions: Seq[KeyValue[_]]) extends Patch
case class OrderChildrenPatch(i: ReorderInstruction) extends Patch
case class AndThenPatch(left: Patch, right: Patch) extends Patch

case class ReorderInstruction(moves: Seq[(Int, Int)], removes: Seq[Int])
