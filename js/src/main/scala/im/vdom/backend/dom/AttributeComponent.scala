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
import _root_.org.scalajs.{dom => d}
import Defaults._
import im.vdom.StyleKey
import im.vdom.ActionLists
import im.vdom.backend.AttrHint
import im.vdom.AttrKey

/**
 * Allows lifting `KeyValue` pairs into a function that can manage DOM attributes.
 * Subclass and override and define a new Backend to use your attribute hints
 * and side-effecting functions.
 *
 * This layer does not automatically run named cleanup actions.
 */
trait AttributeComponent { self: DOMHints with DelegateComponent with ActionLists[d.Node] =>

  /**
   * Call `Element.setAttribute()` with an optional namespace.
   */
  protected def setAttribute(el: d.Element, name: String,
    value: String, namespace: Option[String] = None): Unit = {
    namespace.fold(el.setAttribute(name, value))(ns => el.setAttributeNS(ns, name, value))
  }

  protected def attr(node: d.Node, kv: KeyValue[_]): Unit = {
    val el = node.asInstanceOf[d.Element]
    val name = kv.key.name
    val hints: AttrHint = attrHint(name).getOrElse(Hints.EmptyAttrHints)
    kv.value.fold(el.removeAttribute(name)) { v =>
      if (hints.values(Hints.MustUseAttribute))
        setAttribute(el, name, v.toString, kv.key.namespace)
      else
        el.asInstanceOf[js.Dynamic].updateDynamic(name)(v.asInstanceOf[js.Any])
    }
  }

  protected def style(node: d.Node, kv: KeyValue[_]): Unit = {
    val name = kv.key.name
    val style = node.asInstanceOf[d.html.Element].style
    kv.value.map(_.toString).fold[Unit](style.removeProperty(name))(v => style.setProperty(name, v, ""))
  }

  /**
   * Add a `Delegate` to a DOM node, if needed, to hande the kv event handler. If
   * the handler is None, removes the event type handler but leaves the Delegate.
   * Skips adding the handler if it already has been attached.
   *
   * An action is added to remove the Delegate as part of a cleanup activity.
   */
  protected def handler(node: d.Node, key: FunctionKey, value: Option[FunctionValue]): Unit = {
    import events._
    
    val name = key.name
 
     def addit(d: Delegate, v: FunctionValue) = {
      val cancelable = d.on(name, v.handler, v.matcher, v.useCapture)
      // When this attribute, reperesenting an event, is about to be reset,
      // remove this handler so its not registered twice.
      addAction(name, node, Action.lift {
        cancelable.cancel
        node
      })
      linkDelegate(d, node)
      cancelable
    }

    value.fold[Unit] {
      getDelegate(node).fold() { d => d.off(Some(name)) } // turn off listening for "name" event
    } { v =>
      getDelegate(node).fold {
        val cancelable = addit(Delegate(), v) // create new Delegate
        cancelable.delegate.root(Some(node)) // set the node Delegate listens to
      } { delegate =>
        addit(delegate, v).delegate // use existing Delegate
      }
    }
  }

  /**
   * Pure side effect. This can be easily mapped or wrapped into an IOAction.
   */
  trait KeyValueAction extends (d.Node => Unit)

  /** Lift KeyValue to KeyValueAction */
  implicit def toKeyValueAction(kv: KeyValue[_]): KeyValueAction = {
    kv match {
      case kv@KeyValue(AttrKey(_, _), _) => new KeyValueAction {
        def apply(target: d.Node): Unit = attr(target, kv)
      }
      case kv@KeyValue(StyleKey(_), _) => new KeyValueAction {
        def apply(target: d.Node): Unit = style(target, kv)
      }
      case kv@KeyValue(key@FunctionKey(_), optv) => new KeyValueAction {
        def apply(target: d.Node): Unit = handler(target, key, optv.asInstanceOf[Option[FunctionValue]])
      }
      case _ => throw new IllegalArgumentException("Unknown KeyValue KeyPart")
    }
  }
}

