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
  def :=[T](v: Option[T]): KeyValue[T] = KeyValue[T](self, v)
  def :=[T](v: T): KeyValue[T] = :=[T](Some(v))
}

/**
 * Property that is part of a style.
 */
case class StyleKey(val name: String) extends KeyPart { self =>
  def :=[T](v: Option[T]): KeyValue[T] = KeyValue[T](self, v)
  def :=[T](v: T): KeyValue[T] = :=[T](Some(v))
}

/**
 * Combination of keys and values. A value of None should indicate
 * that something should be unset or removed. What unset or remove
 * means is Backend dependent.
 *
 * The key is parameterized so we can pattern match off of it.
 */
case class KeyValue[T](val key: KeyPart, val value: Option[T]) {
  /**
   * Convenience function to unset or remove a value.
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
 * Add a queue to an object for named cleanup IOActions. The actions can be
 * run by calling `cleanup`.
 *
 */
trait CleanupActions[T] {
  /**
   * Add a cleanup action to run just before an attribute's value is set
   * to a new value. The specified actions are run after already registered actions.
   */
  def addCleanupAction(keyName: String, node: T, action: IOAction[_]*): Unit
  /**
   * Add a cleanup action to run after the Node is removed from the DOM.
   * The specified actions are run after the already registered actions.
   */
  def addCleanupAction(node: T, action: IOAction[_]*): Unit
  /**
   * Run the cleanup actions for el and named. This is run automatically by the PatchesComponent
   * at the right time of the lifecycle. Reset queues. The named queues are run first
   * then the node level queue.
   */
  def cleanup(node: T): Unit

  /** Run the cleanup actions for the named queue. */
  def cleanup(name: String, node: T): Unit
}
