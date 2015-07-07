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

import scalajs.js
import js._
import org.scalajs.dom
import scala.scalajs.js.UndefOr
import UndefOr._

/**
 * Patchs are actions applied to an Element that produce a monad.
 * The monad can be run to execute the action against the Element
 * at a later time. Patches are a recipe for describing changes
 * to an Element.
 */
object PatchModule {

  /**
   * When applied, performs the patch action. Return the result
   * of the action. A patch action will return an element
   * appropriate to the patch type. See the subclass documentation.
   */
  trait PatchAction extends (() => UndefOr[dom.Node]) {

    /** Sequence `after` after this action runs. */
    def andThen(after: PatchAction): PatchAction =
      if (this == NoAction)
        after
      else if (after == NoAction)
        this
      else
        AndThenAction(this, after)

    /** Alias for andThen */
    def >>(after: PatchAction) = andThen(after)

    /** Alias for andThen */
    def +(after: PatchAction) = andThen(after)

    /** Alias for apply. */
    def run = this()
  }

  object PatchAction {
    /**
     * Create a new `PatchAction`.
     */
    def apply(f: => UndefOr[dom.Node]) = new PatchAction { def apply() = f }
  }

  /** An action that does not perform any action and returns js.undefined. */
  val NoAction = PatchAction(js.undefined)

  /** Run the first action then the second action. Return the second action's result. */
  case class AndThenAction(first: PatchAction, second: PatchAction) extends PatchAction {
    def apply() = {
      first()
      second()
    }
  }

  /**
   * A Patch is a builder that creates a `PatctAction` that is run
   * to execute the patch.
   */
  trait Patch extends (dom.Node => PatchAction) {
    /** Compose the patch with another */
    def andThen(after: Patch): Patch = AndThenPatch(this, after)

    /** Alias for andThen */
    def >>(after: Patch) = andThen(after)

    /** Alias for andThen */
    def +(after: Patch) = andThen(after)

    /**
     * Route this path to another part of the tree when its run.
     */
    def applyTo(path: Seq[Int] = Nil) = PathPatch(this, path)

    /**
     * Route this path to a specific child index when it is run.
     */
    def applyTo(path: Int) = PathPatch(this, Seq(path))
  }

  object Patch {
    /** Short cut for creating a patch. Try to use the typed patch classes instead */
    def apply(f: dom.Node => PatchAction) =
      new Patch {
        def apply(el: dom.Node) = f(el)
      }

    /**
     * Create a patch at a specific node in the tree hierarchy.
     */
    def applyTo(f: dom.Node => PatchAction, p: Seq[Int] = Nil) = PathPatch(Patch(f), p)

    /**
     * Create a patch at a specific node in the tree hierarchy.
     */
    def applyTo(f: dom.Node => PatchAction, p: Int) = PathPatch(Patch(f), Seq(p))
  }

  /**
   * Unit
   * Apply a patch to a sub-node indicated by the children path.
   * A path of Nil indicates the current object.
   */
  case class PathPatch(patch: Patch, path: Seq[Int] = Nil) extends Patch {
    def apply(target: dom.Node) = PatchAction {
      path match {
        case Nil => patch(target).run
        case _ => routeToAndPatch(target, patch, path).run
      }
    }
  }

  private[this] def routeToAndPatch(target: dom.Node, patch: Patch, path: Seq[Int] = Nil): PatchAction = {
    path match {
      case Nil => patch(target)
      case head :: tail =>
        if (target.childNodes.length == 0)
          throw new IllegalArgumentException(s"Target node $target does not have children but was routing path $path")
        require(target.childNodes(head) != null)
        routeToAndPatch(target.childNodes(head), patch, path.drop(1))
    }
  }

  /**
   * Apply to first then second.
   */
  case class AndThenPatch(first: Patch, second: Patch) extends Patch {
    def apply(target: dom.Node): PatchAction = first(target) andThen second(target)
  }

  /** A patch that produces a `NoAction`. */
  case object EmptyPatch extends Patch {
    def apply(target: dom.Node): PatchAction = NoAction
  }

  /**
   *  Patch that replaces a target node.
   */
  case class ReplacePatch(val replacement: VNode) extends Patch {
    def apply(target: dom.Node) = PatchAction {
      {
        val y = replacement.render
        for {
          parent <- target.parentOpt
          newNode <- y
        } yield {
          parent.replaceChild(newNode, target)
        }
        y.asInstanceOf[dom.Element]
      }
    }
  }

  /** Patch that removes the target node. */
  case class RemovePatch() extends Patch {

    def apply(target: dom.Node) = PatchAction {
      {
        require(target != null)
        if (target != js.undefined) {
          //println(s"removing target node: $target")
          target.parentOpt.foreach { p =>
            p.removeChild(target)
          }
        }
        scalajs.js.undefined
      }
    }
  }

  /**
   *  Patch that inserts a rendered VNode as a child of the target node.
   *  The insertion uses `appendChild`.
   */
  case class InsertPatch(vnode: VNode) extends Patch {

    def apply(target: dom.Node) = PatchAction {
      {
        require(target != null)
        val x = vnode.render
        x.foreach(target.appendChild(_))
        target
      }
    }
  }

  /** Patch that adds text as a child */
  case class TextPatch(content: String) extends Patch {
    def apply(el: dom.Node) = PatchAction {
      {
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
          t2
        }
      }
    }
  }

  /** Apply multiple actions to an Element. */
  case class MultipleActionPatch(attributes: Attrs) extends Patch {
    def apply(el: dom.Node) = PatchAction {
      {
        require(el != null)
        val ele = el.asInstanceOf[dom.Element]
        attributes.foreach(a => a(ele))
        el
      }
    }
  }

  /** Apply a single el action to an Element. */
  case class SingleActionPatch(elAction: ElementAction) extends Patch {
    def apply(el: dom.Node) = PatchAction {
      val ele = el.asInstanceOf[dom.Element]
      elAction(ele)
      el
    }
  }

  /** Auto promote an `ElementAction` to a `Patch` */
  implicit def EAToSAP(elAction: ElementAction) = SingleActionPatch(elAction)

  case class ReorderInstruction(moves: Seq[(Int, Int)], removes: Seq[Int])

  /** Patch that reoders (removes and moves) the children in the target node. */
  case class OrderChildrenPatch(instructions: ReorderInstruction) extends Patch {
    def apply(el: dom.Node): PatchAction = PatchAction {
      // Process removes correctly, don't delete by index, delete by node object.
      instructions.removes.map(el.childNodes(_)).foreach(el.removeChild(_))
      // Process moves !?!?
      // ...
      js.undefined
    }
  }
}
