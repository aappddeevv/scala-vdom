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
 * You can create your own DSL to create patches if you want.
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

/**
 * Do not do anything. This may translate into some type of "dummy" element.
 */
case object EmptyPatch extends Patch

/**
 * Apply a patch to a particular tree child. Indexes navigate children.
 */
case class PathPatch(patch: Patch, path: Seq[Int] = Nil) extends Patch

/**
 * Remove a node.
 */
case object RemovePatch extends Patch

/**
 * Replace a node. Access to the parent will be required.
 */
case class ReplacePatch(replacement: VNode) extends Patch

/** Insert a new child at the specified index, or append if index is not specified. */
case class InsertPatch(vnode: VNode, index: Option[Int] = None) extends Patch

/** Create a text node. */
case class TextPatch(content: String) extends Patch

/**
 * Apply attributes/properties to an element.
 */
case class KeyValuePatch(elActions: Seq[KeyValue[_]]) extends Patch

/** Manipulate children. */
case class OrderChildrenPatch(i: ReorderInstruction) extends Patch

/**
 * Combine two patches in sequence.
 */
case class AndThenPatch(left: Patch, right: Patch) extends Patch

/**
 * Instruction to re-order children. Removes should be processed first
 * then the moves. Duplicate indexes in any of these structures could
 * produce surprises.
 */
case class ReorderInstruction(moves: Seq[(Int, Int)] = Seq(), removes: Seq[Int] = Seq())
