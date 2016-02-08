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
package vdom
package component

import _root_.org.scalajs.{dom => d}

/**
 * Basic state requires a DOM node and a virtual tree node (VNode).
 */
trait State {
  val node: d.Node
  val tree: VNode
}

/**
 * Component type that links together a method to create virtual trees,
 * the native DOM node (the peer) and other parts needed for managing
 * rendering.
 */
trait Component[S <: State] extends (S => (VNode, S)) {
  def map(f: VNode => VNode) =
    (state: S) => {
      val (vnode, newState) = this(state)
      (f(vnode), newState)
    }
  def flatMap(f: VNode => Component[S]) =
    (state: S) => {
      val (vnode, newState) = this(state)
      f(vnode)(newState)
    }
}

object Component {

  /**
   * Easily create a state from a function.
   */
  def apply[S <: State](r: S => (VNode, S)): Component[S] =
    new Component[S] {
      def apply(s: S) = r(s)
    }

  protected[this] def internalGetID(node: d.Node): Option[NodeIDType] = {
    node match {
      case e: d.Element => Some(e.getAttribute(ID_ATTRIBUTE_NAME))
      case _ => None
    }
  }

  /**
   * Get the NodeID for a DOM node. Ensure that caches are in sync
   * if `node` has a NodeID but it has not been cached yet.
   */
  def getID(node: d.Node): Option[NodeIDType] = {
    // extract from node
    val id = internalGetID(node)

    // find in cache
    val cachedNode = for {
      _id <- id
      cachedNode <- nodeCache.get(_id)
    } yield cachedNode

    id
  }

  //
  // A variety of caches, for fast lookup
  //
  val nodeCache: collection.mutable.Map[NodeIDType, Component[_]] =
    collection.mutable.Map()
  val instancesByRootID: collection.mutable.Map[Int, Component[_]] =
    collection.mutable.Map()
  val containerByRootID: collection.mutable.Map[Int, d.Node] =
    collection.mutable.Map()

  /**
   * Register the container. Create root ID if needed.
   */
  def registerContainer(container: d.Node): Unit = {
  }

  /**
   * Render a component into a container.
   */
  def render(component: Component[_], container: d.Node): Unit = {
  }

}