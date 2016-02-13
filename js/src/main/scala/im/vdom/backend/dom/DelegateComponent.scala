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
package backend
package dom

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.queue
import scala.scalajs.js
import scala.scalajs.js._
import scala.scalajs.js.UndefOr
import scala.scalajs.js.UndefOr._
import scala.language._

import _root_.org.scalajs.{ dom => d }

import Defaults._

/**
 * Manage a `Delegate` associated with a single DOM Node.
 * Each DOM node should have at most one Delegate since
 * a Delegate manages many event types.
 *
 * Delegate cleanup is handled by adding a node clean up action.
 *
 * The Delegate is attached under the __delegate property
 * of the node and if a delegate detach action has been added on this node
 * already, a property __delegate_cleanupaction property is set to true.
 *
 */
trait DelegateComponent { self: ActionLists[d.Node] =>

  import events._
  import js.DynamicImplicits.truthValue

  /**
   * Set `node.__delegate` to js.undefined. Set Delegate's root to None.
   */
  def rmDelegate(dnode: d.Node): d.Node = {
    getDelegate(dnode).foreach { d =>
      d.root(None) // turn everything off
      dnode.asInstanceOf[js.Dynamic].__delegate = js.undefined.asInstanceOf[js.Any]
    }
    dnode
  }

  /**
   * Link a Delegate to a DOM Node. It replaces any previous
   * Delegate linkage that may exist. Adds a cleanup action
   * to disconnect the delegate unless one has already been added.
   */
  def linkDelegate(del: Delegate, dnode: d.Node): Unit = {
    require(dnode != null)
    val dyndnode = dnode.asInstanceOf[js.Dynamic]
    dyndnode.__delegate = del.asInstanceOf[js.Any]
    if (!truthValue(dyndnode.__delegate_cleanupaction)) {
      addDetachAction(dnode, cleanupAction(dnode))
      dyndnode.__delegate_cleanupaction = true
    }
  }

  /**
   * Get an optional Delegate linked to a DOM node.
   */
  def getDelegate(dnode: d.Node): Option[Delegate] = {
    require(dnode != null)
    val x = dnode.asInstanceOf[js.Dynamic].__delegate
    if (truthValue(x)) Some(x.asInstanceOf[Delegate])
    else None
  }

  /** Create a cleanup IOAction that calls rmDelegate and returns the node. */
  private def cleanupAction(node: d.Node) = Action.lift { rmDelegate(node) }

}
