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

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Try
import collection.mutable.ArrayBuffer
import org.im.vdom.backend.Backend

/**
 * A debuggable object means you can get debuggable information.
 */
trait Debuggable {
  def getDebugInfo: DebugInfo
}

/**
 * Debug information is just a string for the moment.
 */
case class DebugInfo(name: String, msg: Option[String] = None, children: Seq[IOAction[_]] = Seq())

/**
 * Basic action object using the IO monad pattern. The side effect,
 * for example changing a DOM element, occurs inside the monad. You
 * can represent failure with IOAction so avoid having `R` be another
 * wrapper that represents failure.
 *
 * TODO: Add ExceutionContext as an implicit to handle server side threading.
 */
sealed trait IOAction[+R] extends Debuggable {
  def map[R2](f: R => R2)(implicit executor: ExecutionContext): IOAction[R2] =
    flatMap[R2](r => SuccessAction[R2](f(r)))

  def flatMap[R2](f: R => IOAction[R2])(implicit executor: ExecutionContext): IOAction[R2] =
    FlatMapAction[R2, R](this, f, executor)

  /**
   * Sequence this before next.
   */
  def andThen[R2](next: IOAction[R2]): IOAction[R2] = AndThenAction[R2](this, next)

  /**
   *  Filter the result with p
   */
  final def filter(p: R => Boolean)(implicit executor: ExecutionContext): IOAction[R] = withFilter(p)

  /**
   * Helper for for-comprehensions.
   */
  def withFilter(p: R => Boolean)(implicit executor: ExecutionContext): IOAction[R] =
    flatMap(v => if (p(v)) SuccessAction(v) else throw new NoSuchElementException("IOAction.withFilter failed"))

  /**
   * Run another action after this one that processes a potential failure. The cleanup
   * action is computed from the result of this action.  The cleanup argument is None
   * if this action succeeds or a Some containing this action's failure if it failed.
   * @param keepFailure if the cleanup action fails, keep that failure info. Otherwise, keep the original failure.
   */
  def cleanUp(f: Option[Throwable] => IOAction[_], keepFailure: Boolean = true)(implicit executor: ExecutionContext): IOAction[R] =
    CleanUpAction[R](this, f, keepFailure, executor)

  /**
   * Run an action after this action, whether this action succeeds or fails. If this
   * action fails, always propagate that failure. If this action succeeds and
   * the second action fails, return the second action's failure. This is much like
   * a 'try { ... } finally { ... }` clause.
   */
  def andFinally(a: IOAction[_]): IOAction[R] = cleanUp(_ => a)(Action.sameThreadExecutionContext)

  /**
   * Return an action that contains the throwable that this action failed with as its result.
   * If this action succeeded, the resulting action fails with a NoSuchElementException.
   * This is a projection of the action.
   */
  def failed: IOAction[Throwable] = FailedAction(this)

  /**
   * Convert to a Try. Use cleanup and andFinally first.
   */
  def asTry: IOAction[Try[R]] = AsTryAction[R](this)

  /**
   * Run another action after this action, if it completed successfully, and return the result
   * of both actions. If either of the two actions fails, the resulting action also fails.
   */
  def zip[R2](a: IOAction[R2]): IOAction[(R, R2)] =
    SequenceAction[Any, ArrayBuffer[Any]](Vector(this, a)).map { r =>
      (r(0).asInstanceOf[R], r(1).asInstanceOf[R2])
    }(Action.sameThreadExecutionContext)

}

/**
 * Helper functions.
 */
object Action {

  /** Convert a `Future` to a [[IOAction]]. */
  def from[R](f: Future[R]): IOAction[R] = FutureAction[R](f)

  /** Lift a constant value to a [[IOAction]]. */
  def successful[R](v: R): IOAction[R] = SuccessAction[R](v)

  /** Create a [[IOAction]] that always fails. */
  def failed(t: Throwable): IOAction[Nothing] = FailureAction(t)

  /** Compose actions from the varargs actions, run in sequence using `andThen`, and return Unit. */
  def seq(actions: IOAction[_]*): IOAction[Unit] =
    (actions :+ SuccessAction(())).reduceLeft(_ andThen _).asInstanceOf[IOAction[Unit]]

  /**
   * Create a IOAction that runs some other actions in sequence and combines their results
   * with the given function.
   */
  def fold[T](actions: Seq[IOAction[T]], zero: T)(f: (T, T) => T)(implicit ec: ExecutionContext): IOAction[T] =
    actions.foldLeft[IOAction[T]](Action.successful(zero)) { (za, va) => za.flatMap(z => va.map(v => f(z, v))) }

  /**
   * An ExecutionContext used internally for executing plumbing operations during IOAction
   * composition.
   */
  private[vdom] object sameThreadExecutionContext extends ExecutionContext {
    override def execute(runnable: Runnable): Unit = runnable.run()
    override def reportFailure(t: Throwable): Unit = throw t
  }
  
  /**
   * Lift a function that takes a context.
   */
  def withContext[R, B <: Backend](f: B#Context => R) = ContextualAction(f)
  
  /**
   * Lift a by-name value into an IOAction
   */
  def lift[R, B <: Backend](f: => R) = ContextualAction(f)
}

/**
 * Context used when running an action.
 */
trait ActionContext

/**
 * When applied, performs the patch action. Return the result
 * of the action. This action has access to the Backend's
 * context while executing.
 *
 * @tparam R output after applying patch
 * @tparam B the `Backend`
 *
 */
trait ContextualAction[+R, -B <: Backend] extends IOAction[R] { self =>
  /**
   *  Run this action with a context.
   */
  def run(ctx: B#Context): R
}

/**
 * Create some convenience functions to create `IOAction`s. Generally,
 * a layer in between the programmer and the Backend hides the
 * creation of actions.
 */
object ContextualAction {
  /**
   *  Create a patch action
   */
  def apply[R, B <: Backend](f: B#Context => R) = new ContextualAction[R, B] {
    def run(ctx: B#Context) = f(ctx)
    def getDebugInfo = DebugInfo("contextual patch")
  }

  /**
   * Create a patch action from a simple action. It ignores the context.
   */
  def apply[R, B <: Backend](f: => R) = new ContextualAction[R, B] {
    def run(ctx: B#Context) = f
    def getDebugInfo = DebugInfo("contextual patch")
  }
}

/**
 *  Perform a flatMap using f.
 */
case class FlatMapAction[+R, P](base: IOAction[P], f: P => IOAction[R], executor: ExecutionContext) extends IOAction[R] {
  def getDebugInfo = DebugInfo("flatMap", children = Seq(base))
}

/**
 *  Performs left then right
 */
case class AndThenAction[+R](left: IOAction[_], right: IOAction[R]) extends IOAction[R] {
  def getDebugInfo = DebugInfo("andThen", children = Seq(left, right))
}

/**
 * Clean up with f after a failure.
 */
case class CleanUpAction[+R](base: IOAction[R], f: Option[Throwable] => IOAction[_],
    keepFailure: Boolean, executor: ExecutionContext) extends IOAction[R] {
  def getDebugInfo = DebugInfo("cleanUp", children = Seq(base))
}

/**
 *  An action that returns a constant value.
 */
case class SuccessAction[+R](value: R) extends ContextualAction[R, Backend] {
  def run(ctx: Backend#Context): R = value
  def getDebugInfo = DebugInfo("success", Some(value.toString))
}

/**
 *  An action that fails with the given throwable when run.
 */
case class FailureAction(t: Throwable) extends ContextualAction[Nothing, Backend] {
  def run(ctx: Backend#Context): Nothing = throw t
  def getDebugInfo = DebugInfo("failure", Some(t.toString))
}

/**
 *  An asynchronous DBIOAction that returns the result of a Future.
 */
case class FutureAction[+R](f: Future[R]) extends IOAction[R] {
  def getDebugInfo = DebugInfo("future ", Some(f.toString))
}

/**
 *  Represents a failed action
 */
case class FailedAction(a: IOAction[_]) extends IOAction[Throwable] {
  def getDebugInfo = DebugInfo("failed", children = Seq(a))
}

/**
 * Represents conversion to a Try
 */
case class AsTryAction[+R](a: IOAction[R]) extends IOAction[Try[R]] {
  def getDebugInfo = DebugInfo("asTry", children = Seq(a))
}

import collection.generic._

/** A DBIOAction that represents a `sequence` operation for sequencing in the DBIOAction monad. */
case class SequenceAction[R, +R2](as: IndexedSeq[IOAction[R]])(implicit val cbf: CanBuild[R, R2]) extends IOAction[R2] {
  def getDebugInfo = DebugInfo("sequence", children = as)
}
