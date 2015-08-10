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

import collection.mutable

import Diff._

/**
 * An object that can be diff'd to produce a patch that takes this object into that.
 *
 * We pull the type parameter into an abstract type so we do not have to repeat it
 * everywhere which qucikly becomes inconvienent. You only diff with objects
 * with the same class as yourself. This uses F-bounded polymorphism so that the diff
 * function only works on the same object type it is declared in. You can only
 * diff on the same type as this.
 */
trait Diffable { self =>
  type That >: self.type <: Diffable
  /**
   * Diff this object with another. The other object needs to be the same class.
   * The patch returned should reflect `path` and any additional pathing needed.
   */
  def diff(that: That, path: Seq[Int]): Patch
}

/** A type that can provide a key. */
trait Keyable {
  /** Default key is None */
  def key: Option[VNodeKey] = None
}

/**
 * Virtual node can be diffed and keyed.
 *
 */
sealed trait VNode extends Keyable with Diffable {
  def closeToOrEquals(rhs: VNode): Boolean = VNodeUtils.closeToOrEquals(this, rhs)
}

/**
 * Node composed of text content.
 */
case class VirtualText(text: String) extends VNode {
  type That = VirtualText

  def diff(that: That, path: Seq[Int]): Patch = {
    val r =
      if (text == that) EmptyPatch
      else TextPatch(that.text)
    r.applyTo(path)
  }
}

object VNodeUtils {
  /**
   * Find removed els. Return indexes of removes. Indexes may not be sorted.
   * Uses `closToOrEquals` to perform node comparison.
   */
  def findRemoves[T](source: Seq[T], target: Seq[T])(compare: (T, T) => Boolean): Seq[Int] =
    // add index to each item, create (index, deleted) flags, filter to keep deletes, return only indexes
    source.zipWithIndex.map {
      case (vnode, index) =>
        (index, kindOfContains(target, vnode)(compare))
    }.filterNot(_._2).map(_._1)

  def findRemovesCloseEnough[T] = findRemoves[T](_: Seq[T], _: Seq[T])(closeToOrEquals)

  /**
   * Find added els. Return indexes of adds relative to the target sequence. Indexes may not be sorted.
   */
  def findAdds[T](source: Seq[T], target: Seq[T])(compare: (T, T) => Boolean = closeToOrEquals _): Seq[Int] =
    findRemoves(target, source)(compare)

  def findAddsCloseEnough[T] = findAdds[T](_: Seq[T], _: Seq[T])(closeToOrEquals)

  /**
   * Like "Sequence.contains` but uses `closeToOrEqual` for the test.
   */
  def kindOfContains[T](seq: Seq[T], vnode: T)(compare: (T, T) => Boolean): Boolean =
    seq.find { closeToOrEquals(_, vnode) }.fold(false)(_ => true)

  def kindOfContainsCloseEnough[T] = kindOfContains[T](_: Seq[T], _: T)(closeToOrEquals)

  /**
   * Determine if two nodes are equal using standard equals, or for VirtualElementNodes, close to each other
   * using `VirtualElementNode.closeTo`.
   */
  def closeToOrEquals[T](lhs: T, rhs: T): Boolean = {
    (lhs, rhs) match {
      case (o: VirtualElementNode, t: VirtualElementNode) => t.closeTo(o)
      case p@(_, _) => p._1 == p._2
    }
  }

}

/**
 * Virtual node representing an element. Various hooks and hacks help capture
 * programmer intent while still allowing a DOM node to be properly configured.
 *
 * @param tag the element tag
 * @param properties key-value pairs. Use "attributes" to enforce using get/set-Attribute otherwise
 * property access is used.
 * @param children list of children vnodes
 * @param key a value used to minimize DOM element node creation. Must be unique among siblings.
 */
case class VirtualElementNode(val tag: String,
    val attributes: Seq[KeyValue[_]] = Seq(),
    val children: Seq[VNode] = Seq(),
    override val key: Option[VNodeKey] = None,
    val namespace: Option[String] = None) extends VNode {

  import VNodeUtils._

  type That = VirtualElementNode

  /**
   * Compare to That using only the tag, key and potentially the namespace.
   *
   */
  def closeTo(that: That): Boolean =
    tag == that.tag && (key fuzzyEq that.key) && (namespace fuzzyEq that.namespace)

  /**
   * Diff properties then children and compose the resulting patches.
   */
  def diff(that: That, path: Seq[Int]): Patch = {
    if (closeTo(that)) {
      // diff properties and children
//      println(s"tag: $tag, $key")
      diffProperties(attributes, that.attributes).applyTo(path) andThen 
        diffSeq(children, that.children, path)
    } else {
      // It's not the same node, so just replace it. Very unoptimized :-)
      ReplacePatch(that).applyTo(path)
    }
  }
}

/** An empty node that renders into something that is backend specific. */
case class EmptyNode() extends VNode {
  type That = EmptyNode
  def diff(that: That, path: Seq[Int]) = PathPatch(EmptyPatch, path)
}

/**
 * Control VNode creation from within a VNode. Instead of
 * composing your VTree externally using a function, you
 * can have subtree generation occur inside the VNode itself.
 * This allows you compose tree generation logic
 * to another tree without relying on function composition
 * external to the tree. Yeah, that sounds useless but it is
 * helpful sometimes.
 */
case class ThunkNode(val f: () => VNode) extends VNode {
  type That = ThunkNode
  def diff(that: That, path: Seq[Int]) = Diff.doDiff(f(), that.f(), path)
}

/**
 * A comment node. Sometimes, comments need to be
 * intersted into a rendering process e.g. markup generation.
 */
case class CommentNode(val content: String) extends VNode {
  type That = CommentNode
  def diff(that: That, path: Seq[Int]) = EmptyPatch.applyTo(path)
}

/**
 * Smart constructors.
 */
object VNode {

  /**
   * Create a constant ThunkNode. `f` is evaluated immediately.
   */
  def constant(f: => VNode) = {
    val c = f
    ThunkNode(() => c)
  }

  /**
   * Create a ThunkNode.
   */
  def thunk(f: => VNode) = ThunkNode(() => f)

  /** Create a new virtual text node */
  def tag(text: String) = VirtualElementNode(text)

  /** Create a new virtual element with the given tag */
  def tag(tag: String, attributes: Seq[KeyValue[_]], children: VNode*): VirtualElementNode =
    VirtualElementNode(tag, attributes, children)

  /** Create a new virtual element with the given tag and key */
  def tag(tag: String, key: VNodeKey, attributes: Seq[KeyValue[_]], children: VNode*): VirtualElementNode =
    VirtualElementNode(tag, attributes, children, Some(key))

  /** Create a new virtual element with the given tag and key */
  def tag(tag: String, key: VNodeKey, children: VNode*): VirtualElementNode =
    VirtualElementNode(tag, Seq(), children, Some(key))

  /** Create a new virtual element with the given tag, key and namespace */
  def tag(tag: String, key: Option[VNodeKey], namespace: Option[String], attributes: Seq[KeyValue[_]], children: VNode*): VirtualElementNode =
    VirtualElementNode(tag, attributes, children, key, namespace)

  /** Create a new virtual element with children, but no attributes. */
  def tag(tag: String, children: VNode*): VirtualElementNode =
    VirtualElementNode(tag, Seq(), children)

  /** Create a SVG element. */
  def svg(attributes: Seq[KeyValue[_]], children: VNode*): VirtualElementNode =
    VirtualElementNode("svg", attributes, children, None, Some(Constants.NS.SVG))

  /**
   * An empty VNode.
   */
  val empty = EmptyNode()

  /**
   * Alias for creating a text node.
   */
  def text(content: String) = VirtualText(content)

  /**
   * Insert a comment.
   */
  def comment(content: String) = CommentNode(content)
}
