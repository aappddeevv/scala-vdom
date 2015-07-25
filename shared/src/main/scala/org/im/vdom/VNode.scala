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

import DiffModule._

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
   */
  def diff(that: That): Patch
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
sealed trait VNode extends Diffable with Keyable

/**
 * Node composed of text content.
 */
case class VirtualText(text: String) extends VNode {
  type That = VirtualText

  def diff(that: VirtualText): Patch = {
    val r =
      if (text == that) EmptyPatch
      else TextPatch(that.text)
    //println(s"diffing VirtualText: patch: $r")
    r
  }
}

object Utils {

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
   * The algorithm has almost no other optimizations :-). Actually,
   * it does not contain any optimizations :-)
   */
  def diffChildren(original: Seq[VNode], target: Seq[VNode]): Patch = {
    val tsize = target.size
    if (target.size == 0) {
      // Remove all the original nodes.
      original.zipWithIndex.map {
        case (value, index) => DiffModule.diff(value, VNode.empty).applyTo(Seq(index))
      }
    } else {
      // Remove everything!
      val removes = OrderChildrenPatch(ReorderInstruction(Seq(), 0 to original.length - 1))
      val adds = target.map(InsertPatch(_))
      removes andThen adds
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
 * @param key a value used to minimize DOM element node creation
 */
case class VirtualElementNode(val tag: String,
    val attributes: Seq[KeyValue[_]] = Seq(),
    val children: Seq[VNode] = Seq(),
    override val key: Option[VNodeKey] = None,
    val namespace: Option[String] = None) extends VNode {

  import Utils._

  type That = VirtualElementNode

  /**
   * Diff properties then children and compose the resulting patches.
   */
  def diff(that: VirtualElementNode): Patch = {
    if (tag == that.tag && key === that.key && (namespace fuzzyEq that.namespace)) {
      // diff properties and children
      diffProperties(attributes, that.attributes) andThen diffChildren(children, that.children)
    } else {
      // It's not the same node, so just replace it. Very unoptimized :-)
      ReplacePatch(that)
    }
  }
}

/** An empty node that renders into something that is backend specific. */
case class EmptyNode() extends VNode {
  type That = EmptyNode
  def diff(that: EmptyNode) = EmptyPatch
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
  def diff(that: ThunkNode) = DiffModule.diff(f(), that.f())
}

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
  def vnode(text: String) = VirtualText(text)

  /** Create a new virtual element with the given tag */
  def vnode(tag: String, attributes: Seq[KeyValue[_]], children: VNode*): VirtualElementNode =
    VirtualElementNode(tag, attributes, children)

  /** Create a new virtual element with the given tag and key */
  def vnode(tag: String, key: Option[VNodeKey], attributes: Seq[KeyValue[_]], children: VNode*): VirtualElementNode =
    VirtualElementNode(tag, attributes, children, key)

  /** Create a new virtual element with the given tag and key */
  def vnode(tag: String, key: VNodeKey, attributes: Seq[KeyValue[_]], children: VNode*): VirtualElementNode =
    VirtualElementNode(tag, attributes, children, Some(key))

  /** Create a new virtual element with the given tag, key and namespace */
  def vnode(tag: String, key: Option[VNodeKey], namespace: Option[String], attributes: Seq[KeyValue[_]], children: VNode*): VirtualElementNode =
    VirtualElementNode(tag, attributes, children, key, namespace)

  /** Create a new virtual element with children, but no attributes. */
  def vnode(tag: String, children: VNode*): VirtualElementNode =
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
}
