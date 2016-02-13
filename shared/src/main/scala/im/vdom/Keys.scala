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

import scala.language._

/**
 * Keys are really builders of `KeyValue` objects.
 */
trait KeyPart { self =>
  def name: String
  def namespace: Option[String] = None
}

/**
 * Attribute that is part of an element.
 */
case class AttrKey(val name: String, override val namespace: Option[String] = None) extends KeyPart { self =>
  def :=[T](v: Option[T]): KeyValue[T] = KeyValue[T](this, v)
  def :=[T](v: T): KeyValue[T] = :=[T](Some(v))
}

/**
 * Property that is part of a style.
 */
case class StyleKey(val name: String) extends KeyPart { self =>
  def :=[T](v: Option[T]): KeyValue[T] = KeyValue[T](this, v)
  def :=[T](v: T): KeyValue[T] = :=[T](Some(v))
}

/**
 * Combination of keys and values. A value of None should indicate
 * that something should be unset or removed. What unset or remove
 * means is Backend dependent.
 *
 */
case class KeyValue[T](val key: KeyPart, val value: Option[T]) {
  /**
   * Convenience function to create an unset KeyValue.
   */
  def unset = copy(value = None)
}

/**
 * Create KeyParts easily given a string: `"blahkey".attr` creates
 * an `AttrKey`.
 */
class RichString(val name: String) {
  def attr = AttrKey(name)
  def style = StyleKey(name)
}

/**
 * The exception type in this system.
 */
class VDomException(msg: String, parent: Throwable = null) extends RuntimeException(msg, parent)

/**
 * Add a queue to an object for named IOAction lists. The actions can be
 * run by calling `runActions`. Running actions should be considered a side effect.
 * Once added, an IOAction cannot be removed from the list. The API clearly
 * shows that the object implementing this trait is mutable.
 *
 */
trait ActionLists[T] {
  /**
   * Add a cleanup action to run just before an attribute's value is set
   * to a new value. The specified actions are run after already registered actions.
   */
  def addAction(keyName: String, node: T, action: IOAction[_]*): Unit
  /**
   * Add an action to run after the Node is detached from its document.
   * The specified actions are run after the already registered actions.
   * Detaching should invoke hierarchical action running if T contains a hierarchy.
   * Whether children action lists are run prior to the top nodes action list
   * is implementation dependent.
   */
  def addDetachAction(node: T, action: IOAction[_]*): Unit
  /**
   * Run the detach actions for el and named. The actions are run automatically by the PatchesComponent
   * at the right time of the lifecycle. Reset all action lists. The named lists are run first
   * then the node level queue. Cleanup should recurse through the children nodes as well.
   */
  def runActions(node: T): Unit

  /** Run the cleanup actions for the named queue. */
  def runActions(name: String, node: T): Unit
}
