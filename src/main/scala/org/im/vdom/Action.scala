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
 * Basic action object using the IO monad pattern. The side effect,
 * for example changing a DOM element, occurs inside the monad.
 *
 * TODO: Add ExceutionContext as an implicit to handle server side threading.
 */
sealed trait IOAction[+R] {
  def map[R2](f: R => R2)(implicit executor: ExecutionContext): IOAction[R2] =
    flatMap[R2](r => SuccessAction[R2](f(r)))

  def flatMap[R2](f: R => IOAction[R2])(implicit executor: ExecutionContext): IOAction[R2] =
    FlatMapAction[R2, R](this, f, executor)

  def andThen[R2](next: IOAction[R2]): IOAction[R2] = AndThenAction[R2](this, next)

  /**
   *  Filter the result with p
   */
  final def filter(p: R => Boolean)(implicit executor: ExecutionContext): IOAction[R] = withFilter(p)

  def withFilter(p: R => Boolean)(implicit executor: ExecutionContext): IOAction[R] =
    flatMap(v => if (p(v)) SuccessAction(v) else throw new NoSuchElementException("IOAction.withFilter failed"))

  /**
   * Action that contains the Throwable with which this action failed as its result.
   * If this action succeeded, the resulting action fails with a NoSuchElementException.
   */
  def failed: IOAction[Throwable] = FailedAction(this)
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

  /** Compose actions from the varargs actions and return Unit. */
  def seq(actions: IOAction[_]*): IOAction[Unit] =
    (actions :+ SuccessAction(())).reduceLeft(_ andThen _).asInstanceOf[IOAction[Unit]]

  /**
   * An ExecutionContext used internally for executing plumbing operations during IOAction
   * composition.
   */
  private[vdom] object sameThreadExecutionContext extends ExecutionContext {
    override def execute(runnable: Runnable): Unit = runnable.run()
    override def reportFailure(t: Throwable): Unit = throw t
  }
}

/**
 * Context used when running an action.
 */
trait ActionContext

/**
 * When applied, performs the patch action. Return the result
 * of the action. A PatchAction will return an object 
 * appropriate to the patch type. See the subclass documentation.
 * 
 * @tparam R output after applying patch
 * @tparam B the `Backend`
 *
 */
trait PatchAction[+R, -B <: Backend] extends IOAction[R] { self =>
  /**
   *  Run this action with a context.
   */
  def run(ctx: B#Context): R
}

object PatchAction {
  /**
   *  Create a patch action
   */
  def apply[R, B <: Backend](f: B#Context => R) = new PatchAction[R, B] {
    def run(ctx: B#Context) = f(ctx)
  }
}

/**
 *  Perform a flatMap using f.
 */
case class FlatMapAction[+R, P](base: IOAction[P], f: P => IOAction[R], executor: ExecutionContext) extends IOAction[R]

/**
 *  Performs left then right
 */
case class AndThenAction[+R](left: IOAction[_], right: IOAction[R]) extends IOAction[R]

/**
 *  An action that returns a constant value.
 */
case class SuccessAction[+R](value: R) extends PatchAction[R, Backend] {
  def run(ctx: Backend#Context): R = value
}

/**
 *  An action that fails.
 */
case class FailureAction(t: Throwable) extends PatchAction[Nothing, Backend] {
  def run(ctx: Backend#Context): Nothing = throw t
}

/**
 *  An asynchronous DBIOAction that returns the result of a Future.
 */
case class FutureAction[+R](f: Future[R]) extends IOAction[R]

/**
 *  Represents a failed action
 */
case class FailedAction(a: IOAction[_]) extends IOAction[Throwable]

