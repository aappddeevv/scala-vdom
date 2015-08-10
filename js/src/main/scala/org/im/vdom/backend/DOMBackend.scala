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
 *
 * This layer does not automatically run named cleanup actions.
 */
trait AttributeComponent { self: DOMAttrHints with DelegateComponent with CleanupActions =>

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

  /**
   * Add a `Delegate` to a DOM node, if needed, to hande the kv event handler. If
   * the handler is None, removes the event type handler but leaves the Delegate.
   * Skips adding the handler if it already has been attached.
   *
   * The Delegate should be removed as part of cleanup activity. This is not
   * orchestrated in this method.
   */
  protected def handler(node: dom.Node, kv: KeyValue[FunctionValue]): Unit = {
    import events._

    val name = kv.key.name

    def addit(d: Delegate, v: FunctionValue) = {
      val cancelable = d.on(name, v.handler, v.matcher, v.useCapture)
      // When this attribute, reperesnting an event, is about to be reset,
      // remove this handler so its not registered twice.
      addCleanupAction(name, node, Action.lift {
        cancelable.cancel
        node
      })
      addDelegate(d, node)
      cancelable
    }

    kv.value.fold[Unit] {
      getDelegate(node).fold() { d => d.off(Some(name)) }
    } { v =>
      getDelegate(node).fold {
        val cancelable = addit(Delegate(), v) // create new Delegate
        cancelable.delegate.root(Some(node))
      } { delegate =>
        addit(delegate, v).delegate // use existing Delegate
      }
    }
  }

  /**
   * Pure side effect. This can be easily mapped or wrapped into an IOAction.
   */
  trait KeyValueAction extends (dom.Node => Unit)

  /**
   * Lift a `KeyValue` and return a `KeyValueAction` that is
   * properly configured to be executed.
   */

  implicit def toKeyValueAction(kv: KeyValue[_]): KeyValueAction = {
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
 * TODO: Fix the execution context. Ensure that the render output future is
 * dependent on the children rendering futures.
 */
trait DOMRendererComponent extends RendererComponent {
  self: Backend with AttributeComponent =>

  import DOMEnvironment._

  type RenderOutput = dom.Node

  def render(vnode: VNode)(implicit executor: ExecutionContext): IOAction[RenderOutput] = {

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
        val emptyNode: UndefOr[dom.Element] = createEl(VNode.tag("div"))
        emptyNode.fold[IOAction[dom.Node]](Action.failed(new NoSuchElementException(s"Unable to create node with tag div")))(n => Action.successful(n))
      case ThunkNode(f) => render(f())
      case x@_ => Action.failed(new VDomException(s"Cannot render VNode type $x for $this"))
    }
  }
}

/**
 * DOM specific Patch processing. Includes an implicit to automatically
 * convert Patches to PatchPerformers.
 *
 * In here be dragons.
 */
trait DOMPatchesComponent extends PatchesComponent {
  self: BasicDOMBackend with DOMRendererComponent with AttributeComponent with CleanupActions =>

  type PatchInput = dom.Node
  type PatchOutput = dom.Node

  /** An action that links a VNode to a dom.Node. Runs cleanup actions. Returns dom.Node. */
  private def detach(node: dom.Node): IOAction[PatchOutput] = Action.lift {
    DOMEnvironment.unlink(node)
    cleanup(node)
    node
  }

  /** An action that adds a link between a VNode and dom.Node. Returns dom.Node. */
  private def attach(vnode: VNode, dnode: dom.Node): IOAction[PatchOutput] = Action.lift {
    DOMEnvironment.link(vnode, dnode)
    dnode
  }

  /**
   * Automatically convert a Patch to a PatchPerformer so it can be
   * applied easily to an input.
   */
  implicit def toPatchPerformer(patch: Patch)(implicit executor: ExecutionContext) =
    makeApplicable(patch)(executor)

  /**
   * Convert patches to a runnable patch based on the backend then it can be
   * converted to an IOAction to run. The PatchPerformer's are constructed
   * to return either the target node, the node that was just changed, or the
   * node that was created or changed. This allows you to use combinators
   * to run actions on the object returned from the operation.
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
        // I should not need undefor here, a bad index should throw an exception. It's an API violation.
        // The node indexes change as you delete which makes this harder than you think.
        val childrenToRemove = i.removes.map(target.childNodes(_).asInstanceOf[UndefOr[dom.Node]])
        childrenToRemove.foreach { c: UndefOr[dom.Node] =>
          c.foreach { x =>
            target.removeChild(x)
          }
        }
        // Process moves.
        // Make copy of current nodes and move via the indexes.
        val copyOfCurrent = (0 to target.childNodes.length).map(target.childNodes(_))
        i.moves.foreach {
          case (from, to) =>
            target.childNodes(to) = copyOfCurrent(from)
        }
        val x = childrenToRemove.filter(_.isDefined).map(_.get).map(detach(_))
        Action.seq(x: _*) andThen Action.successful(target)
      }

      /**
       * Run left then right. Return the original input object.
       *
       * TODO: Should this return, somehow, the output of the left and right operations?
       */
      case AndThenPatch(left, right) => {
        val pleft = makeApplicable(left)
        val pright = makeApplicable(right)
        PatchPerformer { target => pleft(target) andThen pright(target) andThen Action.successful(target) }
      }

      case KeyValuePatch(keyValues) => PatchPerformer { target =>
        require(target != null)
        val ele = target.asInstanceOf[dom.Element]
        keyValues.foreach { a =>
          cleanup(a.key.name, target)
          a(ele)
        }
        Action.successful(target)
      }

      case RemovePatch => PatchPerformer { target =>
        require(target != null)
        target.parentUndefOr.fold[IOAction[PatchOutput]](Action.failed(new IllegalArgumentException(s""))) { p =>
          p.removeChild(target)
          detach(target)
        }
      }

      case InsertPatch(vnode, pos) => PatchPerformer { target =>
        require(target != null)
        render(vnode).map { newNode =>
          pos.filter(i => i < target.childNodes.length && i >= 0).fold(target.appendChild(newNode)) { index =>
            target.insertBefore(newNode, target.childNodes(index))
          }
        }.flatMap(attach(vnode, _))
      }

      // Not sure this returns el, but maybe does not matter if text node is always a leaf
      case TextPatch(content) => PatchPerformer { el =>
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
        // replaceChild returns the old node, not the new one! we want the new one!s
        target.parentUndefOr.fold[IOAction[PatchOutput]](Action.failed(
          new IllegalArgumentException(s"No parent for target $target"))) { parent =>
          detach(target) andThen render(replacement).map { n =>
            parent.replaceChild(n, target)
            n
          }.flatMap(attach(replacement, _))
        }
      }

      /**
       * Don't do anything, but pass along the PatchInput object, whatever it was.
       */
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
 * Manage a `Delegate` associated with a single DOM Node.
 * Each DOM node should have at most one Delegate since
 * a Delegate manages many event types.
 *
 * Delegate cleanup is handled by adding a clean up IOAction
 * to the node if one has not already been set.
 */
trait DelegateComponent { self: CleanupActions =>
  import org.im.events._

  /**
   * Set `node.__delegate` to js.undefined. Set Delegate's root to None.
   */
  def rmDelegate(dnode: dom.Node): dom.Node = {
    getDelegate(dnode).fold() { d =>
      d.root(None) // turn everything off
      dnode.asInstanceOf[js.Dynamic].__delegate = js.undefined.asInstanceOf[js.Any]
    }
    dnode
  }

  /**
   * Link a Delegate to a DOM Node. It replaces any previous
   * Delegate linkage that may exist. Add a cleanup action
   * to disconnect the delegate unless one has already been added.
   */
  def addDelegate(d: Delegate, dnode: dom.Node): Unit = {
    val dyndnode = dnode.asInstanceOf[js.Dynamic]
    dyndnode.__delegate = d.asInstanceOf[js.Any]
    if (!js.DynamicImplicits.truthValue(dyndnode.__delegate_cleanupaction)) {
      addCleanupAction(dnode, cleanupAction(dnode))
      dyndnode.__delegate_cleanupaction = true
    }
  }

  /**
   * Get an optional Delegate linked to a DOM node.
   */
  def getDelegate(dnode: dom.Node): Option[Delegate] = {
    val x = dnode.asInstanceOf[js.Dynamic].__delegate
    if (js.DynamicImplicits.truthValue(x)) Some(x.asInstanceOf[Delegate])
    else None
  }

  /** Create a cleanup IOAction that calls rmDelegate and returns the node. */
  def cleanupAction(node: dom.Node) = Action.lift { rmDelegate(node) }

}

/**
 * Add a "queue" to DOM nodes for cleanup actions run when a DOM node is
 * disconnected from the virtual environment or an attribute's value is
 * about to be reset.
 *
 * Cleanup queues are available for the dom.Node as well as each 
 * attibute-named cleanup queues. The attribute nodes are
 * run before the attribute is set to a new value and then the attribute
 * cleanup queue is cleared.
 *
 * TODO: Fuse together el and named cleanup actions into one mechanism.
 */
trait CleanupActions { self: DOMBackend =>

  type CleanupAction = IOAction[_]
  type NamedCleanupActions = Map[String, IOAction[_]]

  /** Sets the action as the new cleanup action. */
  private[this] def setEl(node: dom.Node, action: CleanupAction): Unit =
    node.asInstanceOf[js.Dynamic].__cleanupAction = action.asInstanceOf[js.Any]

  /** Sets the action as the new named cleanup action. */
  private[this] def setNamed(node: dom.Node, namedActions: NamedCleanupActions): Unit =
    node.asInstanceOf[js.Dynamic].__namedCleanupActions = namedActions.asInstanceOf[js.Any]

  /**
   * Add a cleanup action to run just before an attribute's value is set
   * to a new value.
   */
  def addCleanupAction(keyName: String, node: dom.Node, action: IOAction[_]*): Unit = {
    val actions = Action.seq(action: _*)
    val named = getNamedQueues(node).getOrElse(Map())
    val existingAction = named.get(keyName).getOrElse(Action.successful(()))
    val newAction = existingAction andThen actions
    setNamed(node, named + (keyName -> newAction))
  }

  /**
   * Add a cleanup action to run after the Node is removed from the DOM.
   */
  def addCleanupAction(node: dom.Node, action: IOAction[_]*): Unit = {
    val latest = Action.seq(action: _*)
    getElQueue(node).fold(setEl(node, latest))(q => setEl(node, q andThen latest))
  }

  /**
   * Run the cleanup actions for el and named. This is run automatically by the PatchesComponent
   * at the right time of the lifecycle. Reset queues.
   */
  def cleanup(node: dom.Node): Unit = {
    getElQueue(node).fold() { run(_) }
    node.asInstanceOf[js.Dynamic].__cleanupAction = js.undefined.asInstanceOf[js.Any]
    
    getNamedQueues(node).fold() { _.values.foreach(run(_)) }
    node.asInstanceOf[js.Dynamic].__namedCleanupActions = js.undefined.asInstanceOf[js.Any]
  }

  /** Run the cleanup actions for the named queue. */
  def cleanup(name: String, node: dom.Node): Unit = {
    getNamedQueues(node).fold() { _.get(name).foreach(run(_)) }
    // clear only that queue
    getNamedQueues(node).foreach{ a => setNamed(node, a - name) }
  }

  /**
   * Create a `dom.Node => IOAction[dom.Node]` function that can be used to flatMap
   * an existing IOAction. When run, it adds a cleanup method to the dom.Node's cleanup queue.
   * Returns the dom.Node input.
   */
  def cleanup(action: IOAction[_]*): (dom.Node => IOAction[dom.Node]) =
    (node: dom.Node) => Action.lift {
      addCleanupAction(node, action: _*)
      node
    }

  private[this] def getElQueue(node: dom.Node): Option[CleanupAction] = {
    val x = node.asInstanceOf[js.Dynamic].__cleanupAction
    if (js.DynamicImplicits.truthValue(x)) Some(x.asInstanceOf[CleanupAction])
    else None
  }

  private[this] def getNamedQueues(node: dom.Node): Option[NamedCleanupActions] = {
    val x = node.asInstanceOf[js.Dynamic].__namedCleanupActions
    if (js.DynamicImplicits.truthValue(x)) Some(x.asInstanceOf[NamedCleanupActions])
    else None
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
  with DOMRendererComponent with AttributeComponent
  with DOMAttrHints
  with CleanupActions
  with DelegateComponent

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
