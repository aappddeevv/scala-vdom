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
    runActions(node)
    DOMEnvironment.unlink(node)
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
          find(target, path).fold[IOAction[PatchOutput]](
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
            val t2: UndefOr[d.Text] = createText(content)
            replaceRoot(el, t2)
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
}

