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

/**
 * Allows lifting `KeyValue` pairs into a function that can manage DOM attributes.
 * Subclass and override and define a new Backend to use your attribute hints
 * and side-effecting functions.
 *
 * This layer does not automatically run named cleanup actions.
 */
trait AttributeComponent { self: DOMAttrHints with DelegateComponent with ActionLists[d.Node] =>

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
    val hints: AttrHint = hint(name).getOrElse(Hints.EmptyAttrHints)
    kv.value.fold(el.removeAttribute(name)) { v =>
      if (!(hints.values & Hints.MustUseAttribute).isEmpty)
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

/**
 * TODO: Fix the execution context. Ensure that the render output future is
 * dependent on the children rendering futures.
 */
trait RenderToDOMComponent extends RendererComponent {
  self: Backend with AttributeComponent =>

  import DOMEnvironment._

  type RenderOutput = d.Node

  def render(vnode: VNode)(implicit executor: ExecutionContext): IOAction[RenderOutput] = {
    vnode match {
      case v@VirtualText(content) => Action.lift { createText(content) }

      case v@VirtualElementNode(tag, attributes, children, key, namespace) =>
        val makeEl: IOAction[d.Node] = Action.lift {
          val newNode = createEl(v)
          attributes.foreach(_(newNode))
          newNode
        } flatMap { el =>
          val childrenactions = children.map { child =>
            val renderChildAction = render(child)
            // a bit ugly since appendChild is really a side effoct
            renderChildAction.map { c => el.appendChild(c); c }
          }
          // the child render and append won't happen unless its exposed to be run later
          Action.seq(childrenactions: _*) andThen Action.successful(el)
        }
        makeEl

      case EmptyNode() =>
        /** Empty nodes become empty divs */
        Action.lift(createEl(VNode.tag("div")))

      case ThunkNode(f) => render(f())

      case CommentNode(c) =>
        Action.lift(d.document.createComment(c))
    }
  }
}

/**
 * DOM specific Patch processing. Includes an implicit to automatically
 * convert Patches to PatchPerformers.
 *
 * In here be dragons.
 *
 * I'm not sure that we need to link the vnode to the dom node as the
 * events delegate and cleanup queues are independent of the vnode and
 * the linkage is not needed for those two areas.
 */
trait DOMPatchesComponent extends PatchesComponent {
  self: BasicDOMBackend with RenderToDOMComponent with AttributeComponent with ActionLists[d.Node] =>

  type PatchInput = d.Node
  type PatchOutput = d.Node

  /** An action that unlinks a VNode from a d.Node. Runs cleanup actions. Returns the input d.Node. */
  protected def detach(node: d.Node): IOAction[PatchOutput] = Action.lift {
    DOMEnvironment.unlink(node)
    runActions(node)
    node
  }

  /** An action that adds a link between a VNode and d.Node. Returns d.Node. */
  protected def attach(vnode: VNode, dnode: d.Node): IOAction[PatchOutput] = Action.lift {
    DOMEnvironment.link(vnode, dnode)
    dnode
  }

  /**
   * Automatically convert a Patch to a PatchPerformer so it can be
   * applied easily to an input.
   */
  implicit def toPatchPerformer(patch: Patch)(implicit executor: ExecutionContext) =
    makeApplyable(patch)(executor)

  /**
   * Convert patches to a runnable patch based on the backend then it can be
   * converted to an IOAction to run. The PatchPerformer's are constructed
   * to return either the target node, the node that was just changed, or the
   * node that was created or changed. This allows you to use combinators
   * to run actions on the object returned from the operation.
   */
  def makeApplyable(patch: Patch)(implicit executor: ExecutionContext): PatchPerformer = {
    patch match {

      case PathPatch(patch, path) =>
        val pp = makeApplyable(patch)
        PatchPerformer { target =>
          find(target, patch, path).fold[IOAction[PatchOutput]](
            Action.failed(new IllegalArgumentException(s"Unable to find node from $target using path $path")))(
              newTarget => pp(newTarget))
        }

      case OrderChildrenPatch(i) => PatchPerformer { target =>
        require(target != null)
        // Process removes correctly, don't delete by index, delete by node object.
        // I should not need undefor here, a bad index should throw an exception. It's an API violation.
        // The node indexes change as you delete which makes this harder than you think.
        val childrenToRemove = i.removes.map(target.childNodes(_).asInstanceOf[UndefOr[d.Node]])
        childrenToRemove.foreach { c: UndefOr[d.Node] =>
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
        val pleft = makeApplyable(left)
        val pright = makeApplyable(right)
        PatchPerformer { target => pleft(target) andThen pright(target) andThen Action.successful(target) }
      }

      case KeyValuePatch(keyValues) => PatchPerformer { target =>
        require(target != null)
        val ele = target.asInstanceOf[d.Element]
        keyValues.foreach { a =>
          runActions(a.key.name, target) // run cleanup for that key
          a(ele) // then apply the keyvalue to the element to add it back
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
            // if already a text node, just replace it
            val textEl = el.asInstanceOf[d.Text]
            textEl.replaceData(0, textEl.length, content)
            //textEl.data = content
            textEl
          } else {
            val t2: UndefOr[d.Text] = DOMUtils.createText(content)
            DOMUtils.replaceRoot(el, t2)
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
        require(target != null)
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
  private[this] def find(target: d.Node, patch: Patch, path: Seq[Int]): Option[d.Node] = {
    path match {
      case Nil => Some(target)
      case head :: tail =>
        if (target.childNodes.length == 0 || 
            head >= target.childNodes.length ||
            target.childNodes(head) == null) return None
        find(target.childNodes(head), patch, path.drop(1))
    }
  }

}

/**
 * Manage a `Delegate` associated with a single DOM Node.
 * Each DOM node should have at most one Delegate since
 * a Delegate manages many event types.
 *
 * Delegate cleanup is handled by adding a node clean up action.
 *
 * The Delegate is attached under the __delegate property
 * of the node and if a cleanup action has been added on this node
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
trait DOMBackend extends BasicDOMBackend 
  with DOMPatchesComponent
  with RenderToDOMComponent 
  with AttributeComponent
  with DOMAttrHints
  with DOMCleanupActions
  with DelegateComponent
  with ActionLists[d.Node]

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
