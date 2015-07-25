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
package backend

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.runNow
import scala.scalajs.js
import scala.scalajs.js._
import scala.scalajs.js.UndefOr
import scala.scalajs.js.UndefOr._

import org.im.vdom._
import org.scalajs.dom

import vdom._
import vdom.Defaults._

/**
 * Allows lifting `KeyValue` pairs into a function that can manage DOM attributes.
 * Subclass and override and define a new Backend to use your attribute hints
 * and side-effecting functions.
 */
trait DOMAttributeComponent { self: DOMAttrHints =>

  /**
   * Call `Element.setAttribute()` with an optional attribute.
   */
  protected def setAttribute(el: dom.Element, name: String,
    value: String, namespace: Option[String] = None): Unit = {
    namespace.fold(el.setAttribute(name, value))(ns => el.setAttributeNS(ns, name, value))
  }

  protected def attr(node: dom.Node, kv: KeyValue[_]): Unit = {
    val el = node.asInstanceOf[dom.Element]
    val name = kv.key.name
    val hints: AttrHint = hint(name).getOrElse(Hints.EmptyAttrHints)
    kv.value.fold(el.removeAttribute(name)) { v =>
      if (!(hints.values & Hints.MustUseAttribute).isEmpty)
        setAttribute(el, name, v.toString, kv.key.namespace)
      else
        el.asInstanceOf[js.Dynamic].updateDynamic(name)(v.asInstanceOf[js.Any])
    }
  }

  protected def style(node: dom.Node, kv: KeyValue[_]): Unit = {
    val name = kv.key.name
    val style = node.asInstanceOf[dom.html.Element].style
    kv.value.map(_.toString).fold[Unit](style.removeProperty(name))(v => style.setProperty(name, v, ""))
  }

  protected def handler(node: dom.Node, kv: KeyValue[FunctionValue]): Unit = {
    import events._
    val name = kv.key.name
    kv.value.fold {} { v =>
      val d = Delegate()
      d.on(name, v.handler, v.matcher, v.useCapture).root(Some(node))
    }
  }

  /**
   * Pure side effect :-)
   */
  trait KeyValueAction extends (dom.Node => Unit)

  /**
   * Lift a `KeyValue` and return a `KeyValueAction` that is
   * properly configured to be executed.
   */
  implicit def liftKeyValue(kv: KeyValue[_]): KeyValueAction = {
    kv match {
      case kv@KeyValue(AttrKey(_, _), _) => new KeyValueAction {
        def apply(target: dom.Node): Unit = attr(target, kv)
      }
      case kv@KeyValue(StyleKey(_), _) => new KeyValueAction {
        def apply(target: dom.Node) = style(target, kv)
      }
      case kv@KeyValue(FunctionKey(_), _) => new KeyValueAction {
        def apply(target: dom.Node) = handler(target, kv.asInstanceOf[KeyValue[FunctionValue]])
      }
    }
  }
}

/**
 * Utilities for working with the DOM.
 */
object DOMUtils {

  val doc = dom.document

  /**
   * Create a text node.
   */
  def createText(content: String) = doc.createTextNode(content)

  /**
   * Create a DOM element using selected information from description.
   */
  def createEl(description: VirtualElementNode): dom.Element = {
    description.namespace.fold(doc.createElement(description.tag))(ns => doc.createElementNS(ns, description.tag))
  }

}

/**
 * TODO: Fix the execution context. Ensure that the render output future is
 * dependent on the children rendering futures.
 */
trait DOMRendererComponent extends RendererComponent {
  self: Backend with DOMAttributeComponent =>

  import DOMUtils._

  type RenderOutput = dom.Node

  def render(vnode: VNode)(implicit executor: ExecutionContext): IOAction[dom.Node] = {
    val ctx = createContext().getEC(executor)

    vnode match {
      case v@VirtualText(content) => Action.successful(createText(content))
      case v@VirtualElementNode(tag, attributes, children, key, namespace) =>
        val newNode: UndefOr[dom.Element] = createEl(v)
        newNode.foreach { newNode =>
          // apply the properties
          attributes.foreach(_(newNode))
        }

        // recurse and render the children, append to the new node!
        val renderedAppendedChildren = children.map { childVNode =>
          render(childVNode).map { renderedChild =>
            for {
              parent <- newNode
            } yield parent.appendChild(renderedChild)
          }
        }
        // Sequenced so that append happens in the right order :-)
        val t = run(Action.seq(renderedAppendedChildren: _*))

        // Ensure that the new node is returned from the render command
        newNode.fold[IOAction[dom.Node]](Action.failed(new NoSuchElementException(s"Unable to create node with tag $tag")))(n =>
          Action.from(t) andThen Action.successful(n))

      /** Empty nodes become empty divs */
      case EmptyNode() =>
        val emptyNode: UndefOr[dom.Element] = createEl(VNode.vnode("div"))
        emptyNode.fold[IOAction[dom.Node]](Action.failed(new NoSuchElementException(s"Unable to create node with tag div")))(n => Action.successful(n))

      case ThunkNode(f) => render(f())
      case x@_ => Action.failed(new VDomException("Unknown VNode type $x for $this"))
    }
  }
}

/**
 * DOM specific Patch processing. Includes an implicit to automatically
 * convert Patches to PatchPerformers.
 */
trait DOMPatchesComponent extends PatchesComponent {
  self: BasicDOMBackend with DOMRendererComponent with DOMAttributeComponent =>

  type PatchInput = dom.Node
  type PatchOutput = dom.Node

  /**
   * Automatically convert a Patch to a PatchPerformer so it can be
   * applied easily to an input.
   */
  implicit def toPatchPerformer(patch: Patch)(implicit executor: ExecutionContext) =
    makeApplicable(patch)(executor)

  /**
   * Convert patches to a runnable patch based on the backend then it can be
   * converted to an IOAction to run.
   */
  def makeApplicable(patch: Patch)(implicit executor: ExecutionContext): PatchPerformer = {

    patch match {

      case PathPatch(patch, path) =>
        val pp = makeApplicable(patch)
        PatchPerformer { target =>
          find(target, patch, path).fold[IOAction[PatchOutput]](
            Action.failed(new IllegalArgumentException(s"Unable to find node from $target using path $path")))(
              newTarget => pp(newTarget))
        }

      case OrderChildrenPatch(i) => PatchPerformer { target =>
        // Process removes correctly, don't delete by index, delete by node object.
        val childrenToRemove = i.removes.map(target.childNodes(_).asInstanceOf[UndefOr[dom.Node]])
        childrenToRemove.foreach { c: UndefOr[dom.Node] =>
          c.foreach(target.removeChild(_))
        }
        // Process moves !?!?
        // ...
        Action.successful(target)
      }
      case AndThenPatch(left, right) => {
        val pleft = makeApplicable(left)
        val pright = makeApplicable(right)
        PatchPerformer { target => pleft(target) andThen pright(target) }
      }

      case KeyValuePatch(keyValues) => PatchPerformer { target =>
        require(target != null)
        val ele = target.asInstanceOf[dom.Element]
        keyValues.foreach(a => a(ele))
        Action.successful(target)
      }

      case RemovePatch => PatchPerformer { target =>
        require(target != null)
        target.parentUndefOr.fold[IOAction[PatchOutput]](Action.failed(new IllegalArgumentException(s""))) { p =>
          p.removeChild(target)
          Action.successful(target)
        }
      }

      case InsertPatch(vnode) => PatchPerformer { target =>
        require(target != null)
        render(vnode).map { target.appendChild(_) }
      }

      case TextPatch(content) =>
        PatchPerformer { el =>
          require(el != null)
          Action.successful {
            if (el.nodeType == 3) {
              val textEl = el.asInstanceOf[dom.Text]
              textEl.replaceData(0, textEl.length, content)
              //textEl.data = content
              textEl
            } else {
              val t2: UndefOr[dom.Text] = dom.document.createTextNode(content)
              val x = for {
                p <- el.parentUndefOr
                t <- t2
              } yield p.replaceChild(t, el)
              t2.get
            }
          }
        }

      /**
       * If a target has a parent, replace target with the
       * replacement. Otherwise, throw an exception. Return
       * the new node that replaced the target node.
       */
      case ReplacePatch(replacement) => PatchPerformer { target =>
        // replaceChild returns the old node, not the new one!
        target.parentUndefOr.fold[IOAction[PatchOutput]](Action.failed(
          new IllegalArgumentException(s"No parent for target $target"))) { parent =>
          render(replacement).map { n =>
            parent.replaceChild(n, target)
            n
          }
        }
      }

      case EmptyPatch => PatchPerformer { Action.successful(_) }
    }
  }

  /**
   * Find a node by navigating through the children based on the child indexes.
   * Return None if no node is found, and hence, the path was not in alignment
   * with the actual child structure.
   */
  private[this] def find(target: dom.Node, patch: Patch, path: Seq[Int]): Option[dom.Node] = {
    path match {
      case Nil => Some(target)
      case head :: tail =>
        if (target.childNodes.length == 0 || target.childNodes(head) == null) return None
        find(target.childNodes(head), patch, path.drop(1))
    }
  }

}

/**
 * A base DOM backend that can be extended with specific components as needed.
 * This trait defines the context type.
 */
trait BasicDOMBackend extends Backend {
  type This = BasicDOMBackend
  type Context = BasicContext

  protected[this] def createContext() = new BasicContext {}
}

/**
 * An example of extending the basic backend with your components.
 */
trait DOMBackend extends BasicDOMBackend with DOMPatchesComponent
  with DOMRendererComponent with DOMAttributeComponent with DOMAttrHints

/**
 * Use this backend when a DOM patching process is desired.
 */
object DOMBackend extends DOMBackend


/**
Note: https://groups.google.com/forum/#!topic/scala-js/qlUJWSQ6ccE

No Brendon is right: `if (this._map)` tests whether `this._map` is falsy.
To test that according to JS semantics, you can use

js.DynamicImplicits.truthValue(self._map.asInstanceOf[js.Dynamic])

which returns false if and only if `self._map` is falsy. This pattern should typically be avoided in Scala code, unless transliterating from JS.

Cheers,
Sébastien

*/