/* Copyright 2015 Devon Miller
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
package backend

import scalajs.js
import org.scalajs.dom

/**
 * Utilities to help smooth through the different ways DOM environments
 * handle corner cases. Accessing this module forces some minor
 * detection processing to occur.
 *
 */
trait DOMUtils {

  private val doc = dom.document

  /**
   * Create a text node.
   */
  def createText(content: String) = doc.createTextNode(content)

  /**
   * Create a DOM element using selected information from description.
   * This function creates the element but does not perform any other
   * initialization processing e.g. setting attributes.
   */
  def createEl(description: VirtualElementNode): dom.Element = {
    description.namespace.fold(doc.createElement(description.tag))(ns => doc.createElementNS(ns, description.tag))
  }

  //  val (isWebKit, isFirefox, isTrident) = {
  //    val uagent = dom.window.navigator.userAgent
  //    (uagent.contains("WebKit"), uagent.contains("Firefox"),
  //      uagent.indexOf("Trident"))
  //  }

  val (normalizes, ignoresEmptyText) = {
    //    var p: dom.Element = dom.document.createElement("p")
    //    p.appendChild(dom.document.createTextNode("a"))
    //    p.insertAdjacentHTML("beforeend", "b")
    //    val normalizes = p.childNodes.length == 1

    //    var p = dom.document.createElement("p")
    //    p.appendChild(dom.document.createTextNode(""))
    //    p.insertAdjacentHTML("beforeend", "<b>")
    //    val ignoresEmptyText = p.firstChild.nodeType != 3
    //    (normalizes, ignoresEmptyText)
    (true, true)
  }

  /**
   * Return falsy value as if calling javascript's `object.member` to see if an object
   * has a method or property on it.
   */
  def has(node: dom.Node, property: String): Boolean =
    js.DynamicImplicits.truthValue(node.asInstanceOf[js.Dynamic](property).asInstanceOf[js.Dynamic])

  /** Remove all children in the node. */
  def removeChildren(node: dom.Node): dom.Node = {
    import scala.scalajs.js.JSConverters._
    assert(node != null)
    for(i <- 0 until node.childNodes.length)
      node.removeChild(node.childNodes(i))
      node
  }

}

object DOMUtils extends DOMUtils

/**
 * Link VNodes and DOM Nodes. This can be implemented either in a global map
 * or by sticking the vnode into the DOM node using a secret property. The
 * secret property is `__vnode`.
 *
 * Note: truthy test: js.DynamicImplicits.truthValue(self._map.asInstanceOf[js.Dynamic])
 */
trait DOMInstanceMap {
  import org.im.events._

  /**
   * Link a VNode to a DOM Node.
   */
  def link(vnode: VNode, dnode: dom.Node): Unit = {
    dnode.asInstanceOf[js.Dynamic].__vnode = vnode.asInstanceOf[js.Any]
  }

  /**
   * Unlink a VNode and DOM Node.
   */
  def unlink(dnode: dom.Node): Unit = {
    dnode.asInstanceOf[js.Dynamic].__vnode = js.undefined.asInstanceOf[js.Any]
  }

  /**
   * Get the linked VNode from a DOM Node, if one was linked previously.
   */
  def getVNode(dnode: dom.Node): Option[VNode] = {
    val x = dnode.asInstanceOf[js.Dynamic].__vnode
    if (js.DynamicImplicits.truthValue(x)) Some(x.asInstanceOf[VNode])
    None
  }

}

trait DOMEnvironment extends DOMInstanceMap with DOMUtils

/**
 * Singleton that holds virtual DOM state. Mutable state lives somewhere :-)
 */
object DOMEnvironment extends DOMEnvironment
