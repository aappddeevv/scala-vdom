/* Copyright 2015 Devon Miller
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

import scala.language._
import org.scalatest._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

/**
 * A base DOM backend that can be extended with specific components as needed.
 * This trait defines the context type.
 */
trait TestBackend extends Backend {
  type This = TestBackend
  type Context = BasicContext

  protected[this] def createContext() = new BasicContext {}
}

// Could just do `new TestBackend {}`
class MyTestBackend extends TestBackend

/**
 * A test backend is used to test IOAction run funcitonality.
 */
class BackendSpec extends FlatSpec
    with Assertions
    with Matchers
    with OptionValues {

  val b = new MyTestBackend()

  "Backend" should "run a successful action" in {
    val action = Action.successful(true)
    val result = b.run(action)
    assertResult(true)(Await.result(result, 1 seconds))
  }

  it should "contain the exception when projecting with action.failed " in {
    val ex = new IllegalArgumentException("bad arg")
    val action = Action.failed(ex)
    val result = b.run(action)
    assertResult(ex)(Await.result(result.failed, 1 seconds))
  }

  it should "throw an exception when the action in action.failed does not contain an exception" in {
    val a = Action.successful(10)
    intercept[NoSuchElementException](Await.result(b.run(a.failed), 1 seconds))
  }

  it should "return a future value" in {
    val f = Future(1)
    val action = Action.from(f)
    val result = b.run(action)
    assertResult(1)(Await.result(result, 1 seconds))
  }

  it should "map" in {
    val action = Action.successful(1).map(_ + 1)
    val result = b.run(action)
    assertResult(2)(Await.result(result, 1 seconds))
  }

  it should "flatMap" in {
    val action = Action.successful(1)
    val action2 = action.flatMap(n => Action.successful(n + 1))
    val result = b.run(action2)
    assertResult(2)(Await.result(result, 1 seconds))
  }

  it should "sequence actions, run them, return Unit" in {
    val r = new AtomicInteger(0)
    val actions = (1 to 10).map(n => ContextualAction {
      r.incrementAndGet()
      n
    })
    val actions2 = (1 to 10).map(n =>
      ContextualAction[Int, MyTestBackend] { ctx: MyTestBackend#Context =>
        r.incrementAndGet()
        n
      })
    val action = Action.seq(actions: _*)
    val result = b.run(action)
    assertResult(())(Await.result(result, 1 seconds))
    assertResult(10)(r.get)

    val action2 = Action.seq(actions2: _*)
    val result2 = b.run(action2)
    assertResult(())(Await.result(result2, 1 seconds))
    assertResult(20)(r.get)
  }

  it should "allow easy extraction to a value" in {
    val x = Action.successful(Some(10))
    val y = x.map(_.get)
    val r = b.run(y)
    assertResult(10)(Await.result(r, 1 seconds))

    val extracted = x.flatMap { opt =>
      opt match {
        case Some(x) => Action.successful(x)
        case _ => Action.failed(new NoSuchElementException("no el"))
      }
    }
    assertResult(10)(Await.result(b.run(extracted), 1 seconds))
  }

  it should "always call finally" in {
    val a = Action.successful(10)
    val counter = new AtomicInteger(0)
    val af = a.andFinally(ContextualAction {
      counter.incrementAndGet()
    })
    val r = Await.result(b.run(af), 1 seconds)
    assertResult(1)(counter.get)

    val ex = new IllegalArgumentException("blah")
    val a2 = Action.failed(ex)
    val af2 = a2.andFinally(ContextualAction {
      counter.incrementAndGet()
    })
    assertResult(ex)(Await.result(b.run(af2.failed), 1 seconds))
    assertResult(2)(counter.get)
  }

  it should "propagate base actions failure when using finally" in {
    val a: IOAction[Int] = Action.failed(new IllegalArgumentException("blah"))
    val counter = new AtomicInteger(0)
    val af = a.andFinally(ContextualAction {
      counter.incrementAndGet()
    })
    val f = b.run(af.failed)
    val r = Await.result(f, 1 seconds)
    assertResult(1)(counter.get)
  }

  it should "call cleanup when successful" in {
    val x = Action.successful(Some(10))
    val y = x.map(_.get)
    val r = b.run(y)
    val counter = new AtomicInteger(0)

    val cleanup = y.cleanUp { err: Option[Throwable] =>
      err match {
        case Some(t) =>
          fail("should not be called")
        case _ =>
          counter.incrementAndGet()
          Action.successful(-1)
      }
    }
    assertResult(10)(Await.result(b.run(cleanup), 1 seconds))
    assertResult(1)(counter.get)
  }

  it should "pass exception from base action to cleanup" in {
    val ex = new IllegalArgumentException("ouch!")
    val baseAction: IOAction[Int] = Action.failed(ex)
    val counter = new AtomicInteger(0)

    val cleanup2: IOAction[Int] = baseAction
      .cleanUp { err: Option[Throwable] =>
        err match {
          case Some(t) =>
            counter.incrementAndGet()
            Action.successful(())
          case _ =>
            fail("should not be called")
        }
      }
    val x = Await.result(b.run(cleanup2).failed, 1 seconds)
    assertResult(ex)(x)
    assertResult(1)(counter.get)
  }

  it should "return cleanup actions exception if base fails, cleanUp fails and keepFailure=false" in {
    val ex = new IllegalArgumentException("ouch!")
    val baseAction: IOAction[Int] = Action.failed(ex)
    val returnedEx = new NoSuchElementException()
    val cleanup: IOAction[Int] = baseAction.cleanUp({
      _ match {
        case Some(t) => Action.failed(returnedEx)
        case _ => Action.successful(())
      }
    }, false)
    val r = Await.result(b.run(cleanup.failed), 1 seconds)
    assertResult(returnedEx)(r)
  }

}