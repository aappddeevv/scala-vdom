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

import scalajs.js
import js._
import org.scalajs.dom
import scala.scalajs.js.UndefOr
import UndefOr._
import org.im.vdom.AndThenPatch
import org.im.vdom.Backend
import org.im.vdom.EmptyNode
import org.im.vdom.EmptyPatch
import org.im.vdom.InsertPatch
import org.im.vdom.MultipleActionPatch
import org.im.vdom.OrderChildrenPatch
import org.im.vdom.Patch
import org.im.vdom.PatchAction
import org.im.vdom.PatchesComponent
import org.im.vdom.PathPatch
import org.im.vdom.RemovePatch
import org.im.vdom.RendererComponent
import org.im.vdom.ReplacePatch
import org.im.vdom.RichNode
import org.im.vdom.SingleActionPatch
import org.im.vdom.TextPatch
import org.im.vdom.VDomException
import org.im.vdom.VNode
import org.im.vdom.VirtualElementNode
import org.im.vdom.VirtualText


trait DOMRendererComponent extends RendererComponent { self: DOMBackend =>

  type RenderOutput = UndefOr[dom.Node]

  def render(vnode: VNode): UndefOr[dom.Node] = {
    vnode match {
      case VirtualText(content) => dom.document.createTextNode(content)
      case VirtualElementNode(tag, attributes, children, key, namespace) =>
        val newNode: UndefOr[dom.Element] = dom.document.createElement(tag)
        newNode.foreach { newNode =>
          // apply the properties
          attributes.foreach(_(newNode))

          // recurse and create the children, append to the new node!
          children.foreach(childVNode => render(childVNode).foreach(newNode.appendChild(_)))
        }
        newNode
      case EmptyNode() => js.undefined
      case x@_ => throw new VDomException("Unknown VNode type $x for $this")
    }
  }

}

trait DOMPatchesComponent extends PatchesComponent { self: Backend with DOMRendererComponent =>

  type PatchInput = dom.Node

  implicit def toPatchPerformer(patch: Patch): PatchPerformer[dom.Node] =
    patchToPerformer(patch)

  /**
   * Hack to convert patches to a runnable patch based on the backend then to
   * an IOAction to run with a context. We'll migrate this up-class after it
   * works for the basics well. Otherwise, knowledge on how to use the context,
   * if needed, is too far down class.
   */
  protected[this] def patchToPerformer(patch: Patch): PatchPerformer[dom.Node] = {
    patch match {
      case PathPatch(patch, path) =>
        val pp = patchToPerformer(patch)
        new PatchPerformer[dom.Node] {
          def apply(target: dom.Node) = pp(find(target, patch, path))
        }
      case SingleActionPatch(elAction) => new PatchPerformer[dom.Node] {
        def apply(target: dom.Node) = PatchAction[dom.Node, This] { ctx: This#Context =>
          elAction(target.asInstanceOf[dom.Element])
          target
        }
      }
      case OrderChildrenPatch(i) => new PatchPerformer[dom.Node] {
        def apply(target: dom.Node) = PatchAction[dom.Node, This] { ctx: This#Context =>
          // Process removes correctly, don't delete by index, delete by node object.
          i.removes.map(target.childNodes(_)).foreach(target.removeChild(_))
          // Process moves !?!?
          // ...
          target
        }
      }
      case AndThenPatch(left, right) => {
        val pleft = patchToPerformer(left)
        val pright = patchToPerformer(right)
        new PatchPerformer[dom.Node] {
          def apply(target: dom.Node) = pleft(target) andThen pright(target)
        }
      }
      case MultipleActionPatch(elActions) => new PatchPerformer[dom.Node] {
        def apply(target: dom.Node) = PatchAction[dom.Node, This] { ctx: This#Context =>
          {
            require(target != null)
            val ele = target.asInstanceOf[dom.Element]
            elActions.foreach(a => a(ele))
            target
          }
        }
      }
      case RemovePatch() => new PatchPerformer[dom.Node] {
        def apply(target: dom.Node) = PatchAction[dom.Node, This] { ctx: This#Context =>
          {
            require(target != null)
            if (target != js.undefined) {
              //println(s"removing target node: $target")
              target.parentOpt.foreach { p =>
                p.removeChild(target)
              }
            }
            target
          }
        }
      }
      case InsertPatch(vnode) => new PatchPerformer[dom.Node] {
        def apply(target: dom.Node) = PatchAction[dom.Node, This] { ctx: This#Context =>
          {
            require(target != null)
            val x = render(vnode)
            x.foreach(target.appendChild(_))
            target
          }
        }
      }
      case TextPatch(content) => new PatchPerformer[dom.Node] {
        def apply(el: dom.Node) = new PatchAction[dom.Node, This] {
          def run(ctx: This#Context) = {
            require(el != null)
            if (el.nodeType == 3) {
              val textEl = el.asInstanceOf[dom.Text]
              textEl.replaceData(0, textEl.length, content)
              //textEl.data = content
              textEl
            } else {
              val t2: UndefOr[dom.Text] = dom.document.createTextNode(content)
              val x = for {
                p <- el.parentOpt
                t <- t2
              } yield p.replaceChild(t, el)
              t2.get
            }
          }
        }
      }
      case ReplacePatch(replacement) => new PatchPerformer[dom.Node] {
        def apply(target: dom.Node) = new PatchAction[dom.Node, This] {
          def run(ctx: This#Context) = {
            val y = render(replacement)
            for {
              parent <- target.parentOpt
              newNode <- y
            } yield {
              parent.replaceChild(newNode, target)
            }
            y.asInstanceOf[dom.Node]
          }
        }
      }
      case EmptyPatch => new PatchPerformer[dom.Node] {
        def apply(target: dom.Node) = new PatchAction[dom.Node, This] {
          def run(ctx: This#Context) = target
        }
      }
      case x@_ => throw new VDomException("Unknown patch type $x for $this")
    }
  }

  private[this] def find[I <: dom.Node](target: I, patch: Patch, path: Seq[Int]): dom.Node = {
    path match {
      case Nil => target
      case head :: tail =>
        if (target.childNodes.length == 0)
          throw new VDomException(s"Target node $target does not have children but was routing path $path")
        require(target.childNodes(head) != null)
        find(target.childNodes(head), patch, path.drop(1))
    }
  }

}

trait DOMBackend extends Backend with DOMPatchesComponent with DOMRendererComponent {
  type This = DOMBackend
  type Context = BasicContext

  protected[this] def createContext() = new BasicContext {}

}

/**
 * Use this backend when a DOM patching process is desired.
 */
object DOMBackend extends DOMBackend
