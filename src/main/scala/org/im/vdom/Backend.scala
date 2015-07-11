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
import scala.concurrent.{ Promise, ExecutionContext, Future }
import scala.util.{ Try, Success, Failure }
import scala.util.control.NonFatal

/**
 * The exception type in this system.
 */
class VDomException(msg: String, parent: Throwable = null) extends RuntimeException(msg, parent)

/**
 * A backend that can render a VNode, create functions that can take a Patch
 * and allow it to be applied to backend specific object to create an IOAction
 * and run IOActions.
 *
 * Yes, this looks a little like typesafe's slick.
 */
trait Backend extends PatchesComponent with RendererComponent { self =>
  type Context >: Null <: BasicContext
  type This >: this.type <: Backend

  final def run[R](a: IOAction[R]): Future[R] = runInternal(a)

  private[vdom] def runInternal[R](a: IOAction[R]): Future[R] =
    try runInContext(a, createContext()) catch { case NonFatal(ex) => Future.failed(ex) }

  /**
   *  Create the default Context for this backend.
   */
  protected[this] def createContext(): Context

  /**
   * Handle compositional actions and send out patch actions to a subclass overridable
   * method.
   */
  protected[this] def runInContext[R](a: IOAction[R], ctx: Context): Future[R] = {
    logAction(a, ctx)
    a match {
      case FlatMapAction(base, f, ec) => runInContext(base, ctx).flatMap(v => runInContext(f(v), ctx))(ctx.getEC(ec))
      case AndThenAction(a1, a2) => runInContext(a1, ctx).flatMap(_ => runInContext(a2, ctx))(Action.sameThreadExecutionContext)
      case SuccessAction(v) => Future.successful(v)
      case FailureAction(t) => Future.failed(t)
      case FutureAction(f) => f
      case FailedAction(a) => runInContext(a, ctx).failed.asInstanceOf[Future[R]]
      case a: PatchAction[_, _] => runPatchAction(a.asInstanceOf[PatchAction[R, This]], ctx)
      case x@_ => throw new VDomException(s"Unknown action type $x for $this")
    }
  }

  protected[this] def runPatchAction[R](a: PatchAction[R, This], ctx: Context): Future[R] = {
    val promise = Promise[R]()
    try {
      val r = a.run(ctx)
      promise.success(r)
    } catch {
      case NonFatal(ex) => promise.tryFailure(ex)
    }

    promise.future
  }

  protected[this] def logAction(a: IOAction[_], ctx: Context): Unit = {
    ctx.sequenceCounter += 1
    //println(s"#${ctx.sequenceCounter}: $a")
  }

  trait BasicContext extends ActionContext {
    @volatile private[Backend] var sequenceCounter: Long = 0

    /**
     * Given an ExcecutionContext return one, perhaps the modified original or a different one.
     */
    private[Backend] def getEC(ec: ExecutionContext): ExecutionContext = ec
  }
}

/**
 * Backend rendering of VNodes
 */
trait RendererComponent { self: Backend =>

  type RenderOutput
  def render(vnode: VNode): RenderOutput

}

/**
 * Backend dependent Patch execution.
 */
trait PatchesComponent { self: Backend =>
  type PatchInput
  type PatchOutput

  trait PatchPerformer extends (PatchInput => IOAction[PatchOutput])

  object PatchPerformer {
    /** Create PatchPerformers more easily */
    def apply(f: PatchInput => IOAction[PatchOutput]) = 
      new PatchPerformer { def apply(pi: PatchInput) = f(pi) }
  }

  def applyPatch(patch: Patch): PatchPerformer
}
