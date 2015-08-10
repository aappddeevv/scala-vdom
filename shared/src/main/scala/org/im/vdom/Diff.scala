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
 * It's not clear why I can't just build this into the VNode trait
 * and split this out to subclasses versus having this
 * function stuck out here in the middle of nowhere.
 */
private[vdom] trait Diff {

  import VNodeUtils._

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
    doDiff(original, target, Nil)
  }

  def doDiff(original: VNode, target: VNode, path: Seq[Int] = Nil): Patch = {
    if (original == target) EmptyPatch
    else if (original == VNode.empty) InsertPatch(target).applyTo(path)
    else if (target == VNode.empty) RemovePatch.applyTo(path)
    else (original, target) match {
      case (o: VirtualText, t: VirtualText) => o.diff(t, path)
      case (o: VirtualElementNode, t: VirtualElementNode) => o.diff(t, path)
      case (o: ThunkNode, t: ThunkNode) => o.diff(t, path)
      case (o: ThunkNode, _) => doDiff(o.f(), target, path)
      case (_, t) => ReplacePatch(target).applyTo(path)
    }
  }

  /**
   * Totally unoptimized :-). We should sort and see if any
   * attributes are the same, leave those in place and only
   * patch to remove attributes.
   */
  def diffProperties(original: Seq[KeyValue[_]], target: Seq[KeyValue[_]]): Patch = {
    val deletes = original.diff(target).map { x =>
      KeyValuePatch(Seq(x.unset))
    }
    val adds = target.diff(original).map { x =>
      KeyValuePatch(Seq(x))
    }
    deletes ++ adds
  }

  /**
   * Observes keyed VNodes and tries to identify moved nodes.
   * The algorithm has almost no other optimizations :-).
   *
   * Optimizations that do exist: Reorderig children.
   *
   * It does not track keyed objects that are on separate levels.
   */
  def diffSeq(original: Seq[VNode], target: Seq[VNode], path: Seq[Int]): Patch = {
    val tsize = target.size
    if (target.size == 0) {
      // Remove all the original nodes.
      original.zipWithIndex.map {
        case (value, index) => doDiff(value, VNode.empty, path :+ index)
      }
    } else {
      // Find removes
      /*
      println("src:")
      original.foreach(println)
      println("tgt:")
      target.foreach(println)   
      */

      // Find nodes that were removed. Indexes valid against original sequence.
      // Seq(index,...)
      val removedIndexes = findRemovesCloseEnough(original, target)

      // Create tmp original with removes removed
      // Seq((vnode, orig index in original), ...)
      val origLessRemoves =
        original.zipWithIndex.filterNot { case (node, index) => removedIndexes.contains(index) }

      // Find nodes that are pure adds. Indexes are relative to target sequence.
      // Return Seq(index),...)
      val addedIndexes = findAddsCloseEnough(original, target)

      // Create tmp target sequence with "adds" removed.
      // Seq((vnode, orig index in target), ...)
      val targetLessAdded =
        target.zipWithIndex.filterNot { case (node, index) => addedIndexes.contains(index) }

      // Find moved items taking into account keys where available.
      // Operate on original with removes removed and target with adds removed.
      // Remove moves that are to and from the same position since that's not really a move.
      // Seq((original index, target index), ...)
      val moves = origLessRemoves.map { orig =>
        targetLessAdded.indexWhere { tgt =>
          closeToOrEquals(orig._1, tgt._1)
        }
      }.zipWithIndex.filter(_._1 >= 0).filterNot(p => p._1 == p._2)

//      println(s"removed: $removedIndexes")
//      println(s"adds: $addedIndexes")
//      println(s"moves: $moves")
//      println(s"targetLessAdded: $targetLessAdded")

      //
      // For the reduced original sequence, it should patch index-wise to the reduce target
      // sequence. Now we can run a pure diff on those to find any diffs that reflect
      // "close" node definitions. Those that are the same should work out to an empty diff 
      // automatically.
      //
      val origLessRemovesWithMoves = collection.mutable.IndexedSeq(origLessRemoves: _*)
      moves.foreach {
        case (from, to) =>
          origLessRemovesWithMoves(to) = origLessRemoves(from)
      }
//      println(s"olrwm: $origLessRemovesWithMoves")
      assert(origLessRemovesWithMoves.size == targetLessAdded.size)
      val restdiff = origLessRemovesWithMoves.zip(targetLessAdded).zipWithIndex.map {
        case ((l, r), index) =>
          doDiff(l._1, r._1, path :+ index)
      }
//      println(s"child diffs: $restdiff")

      (OrderChildrenPatch(ReorderInstruction(moves, removedIndexes)) andThen
        restdiff andThen
        addedIndexes.map { index => InsertPatch(target(index), Some(index)) }).applyTo(path)

      //      val removes = OrderChildrenPatch(ReorderInstruction(Seq(), 0 to original.length - 1))
      //      val adds = target.map(InsertPatch(_))
      //      removes andThen adds
    }
  }
}

private[vdom] object Diff extends Diff