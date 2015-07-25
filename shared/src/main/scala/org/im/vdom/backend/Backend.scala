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

import scala.concurrent.{ Promise, ExecutionContext, Future }
import scala.util.control.NonFatal
import org.im.vdom.FailedAction
import org.im.vdom.FutureAction
import org.im.vdom.Patch
import org.im.vdom.VNode
import util.{ Success, Failure, Try }

/**
 * A backend that can run IOActions.
 *
 * Yes, this looks a little like typesafe's slick.
 */
trait Backend { self =>
  type Context >: Null <: BasicContext
  protected[this]type This >: this.type <: Backend

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
      case AsTryAction(t) =>
        val p = Promise[R]()
        runInContext(a, ctx).onComplete(v => p.success(v.asInstanceOf[R]))(Action.sameThreadExecutionContext)
        p.future
      case sa@SequenceAction(actions) =>
        import java.util.concurrent.atomic.AtomicReferenceArray
        val len = actions.length
        val results = new AtomicReferenceArray[Any](len)
        def run(pos: Int): Future[Any] = {
          if (pos == len) Future.successful {
            val b = sa.cbf()
            var i = 0
            while (i < len) {
              b += results.get(i)
              i += 1
            }
            b.result()
          }
          else runInContext(actions(pos), ctx).flatMap { (v: Any) =>
            results.set(pos, v)
            run(pos + 1)
          }(Action.sameThreadExecutionContext)
        }
        run(0).asInstanceOf[Future[R]]

      case CleanUpAction(base, f, keepFailure, ec) =>
        val p = Promise[R]()
        runInContext(base, ctx).onComplete { t1 =>
          try {
            // transform base action result to a flipped Option
            val a2 = f(t1 match {
              case Success(_) => None
              case Failure(t) => Some(t)
            })
            // Run the user function that transforms the error
            runInContext(a2, ctx).onComplete { t2 =>
              if (t2.isFailure && (t1.isSuccess || !keepFailure)) p.complete(t2.asInstanceOf[Failure[R]])
              else p.complete(t1)
            }(Action.sameThreadExecutionContext)
          } catch {
            case NonFatal(ex) =>
              throw (t1 match {
                case Failure(t) if keepFailure => t
                case _ => ex
              })
          }
        }(ctx.getEC(ec))
        p.future
      case FailedAction(a) => runInContext(a, ctx).failed.asInstanceOf[Future[R]]
      case a: ContextualAction[_, _] => runPatchAction(a.asInstanceOf[ContextualAction[R, This]], ctx)
      case x@_ => throw new VDomException(s"Unknown action type $x for $this")
    }
  }

  protected[this] def runPatchAction[R](a: ContextualAction[R, This], ctx: Context): Future[R] = {
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
    private[backend] def getEC(ec: ExecutionContext): ExecutionContext = ec
  }
}

/**
 * Backend rendering of VNodes. Since rendering may involve side effects
 * or asynchronous activities as a VNode is converted into the output,
 * return an IOAction wrapper around the output.
 */
trait RendererComponent { self =>

  /**
   * The type output from the rendering.
   */
  type RenderOutput

  /** Render a VNode producing RenderOutput objects. */
  def render(vnode: VNode)(implicit executor: ExecutionContext): IOAction[RenderOutput]
}

/**
 * Convert Patches to functions that can be applied to
 * Backend specific elements to create IOActions to be run
 * by the bBackend.
 */
trait PatchesComponent { self =>
  type PatchInput
  type PatchOutput

  /**
   * Convert PatchInput into a wrapped PatchOutput.
   */
  trait PatchPerformer extends (PatchInput => IOAction[PatchOutput])

  object PatchPerformer {
    /** Create PatchPerformers more easily */
    def apply(f: PatchInput => IOAction[PatchOutput]) =
      new PatchPerformer {
        def apply(pi: PatchInput) = f(pi)
      }
  }

  /**
   * Make a patch able to be applied to `PatchInput` to create
   * a runnable action. Generally, this function must process
   * all Patch types and produce a PatchPerformer object. Concrete
   * backends will need to match against the Patch type and
   * take the appropriate action.
   */
  def makeApplicable(patch: Patch)(implicit executor: ExecutionContext): PatchPerformer
}
