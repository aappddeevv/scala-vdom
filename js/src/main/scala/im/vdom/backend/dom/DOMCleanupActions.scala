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
 * Cleanup actions specific to a DOM backend.
 *
 * Add a "queue" to DOM nodes for cleanup actions that are run when a DOM node is
 * detached from the environment or an attribute's value is about to be changed.
 *
 * Cleanup queues are available for the d.Node as well as each
 * attibute-named cleanup queues. An attribute's queue are
 * run before the attribute is set to a new value and then the attribute
 * cleanup queue is cleared.
 *
 * A node property called __namedCleanupActions is added to the DOM node.
 *
 * Cleanup queues can be used to disconnect event handlers or
 * manager external resources.
 *
 */
trait DOMCleanupActions extends ActionLists[d.Node] { self: DOMBackend =>
  import js.DynamicImplicits.truthValue

  val nodeCleanupQueueName = "__node__"

  type NamedCleanupActions = Map[String, IOAction[_]]

  /** Sets the action as the new named cleanup action. */
  private[this] def setNamed(node: d.Node, namedActions: NamedCleanupActions): Unit =
    node.asInstanceOf[js.Dynamic].__namedCleanupActions = namedActions.asInstanceOf[js.Any]

  private[this] def getNamedQueues(node: d.Node): Option[NamedCleanupActions] = {
    val x = node.asInstanceOf[js.Dynamic].__namedCleanupActions
    if (truthValue(x)) Some(x.asInstanceOf[NamedCleanupActions])
    else None
  }

  /**
   * Add a cleanup action to run just before an attribute's value is set
   * to a new value. The specified actions are run after already registered actions.
   */
  def addAction(keyName: String, node: d.Node, action: IOAction[_]*): Unit = {
    val actions = Action.seq(action: _*)
    val named = getNamedQueues(node).getOrElse(Map())
    val existingAction = named.get(keyName).getOrElse(Action.successful(()))
    val newAction = existingAction andThen actions
    setNamed(node, named + (keyName -> newAction))
  }

  def addDetachAction(node: d.Node, action: IOAction[_]*): Unit = {
    val latest = Action.seq(action: _*)
    addAction(nodeCleanupQueueName, node, latest)
  }

  /**
   * Runs a breadth first pass at running actions. First children recursion,
   * then attributes then the node itself. The action list is set to undefined.
   */
  def runActions(node: d.Node): Unit = {
    getNamedQueues(node).fold() { actionMap =>
      (0 until node.childNodes.length).foreach{ i => runActions(node.childNodes(i)) }
      (actionMap - nodeCleanupQueueName).values.foreach(run(_))
      actionMap.get(nodeCleanupQueueName).foreach(run(_))

    }
    node.asInstanceOf[js.Dynamic].__namedCleanupActions = js.undefined.asInstanceOf[js.Any]
  }

  def runActions(name: String, node: d.Node): Unit = {
    getNamedQueues(node).fold() { _.get(name).foreach(run(_)) }
    // clear only that queue
    getNamedQueues(node).foreach { a => setNamed(node, a - name) }
  }

  /**
   * Create a `d.Node => IOAction[d.Node]` function that can be used to flatMap
   * an existing IOAction. When run, it adds a cleanup method to the d.Node's cleanup queue.
   * Returns the d.Node input.
   */
  private def cleanup(action: IOAction[_]*): (d.Node => IOAction[d.Node]) =
    (node: d.Node) => Action.lift {
      addDetachAction(node, action: _*)
      node
    }
}
