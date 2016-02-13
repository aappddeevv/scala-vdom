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
package im
package vdom
package backend

import scala.language._
import org.scalatest._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
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
class BackendSpec extends AsyncFlatSpec with Matchers with OptionValues {

  val b = new MyTestBackend()

  "Backend" should "run a successful action" in {
    val action = Action.successful(true)
    val result = b.run(action)
    result.map { result =>
      result should be(true)
    }
  }

  it should "contain the exception when projecting with action.failed " in {
    val ex = new IllegalArgumentException("bad arg")
    val action = Action.failed(ex)
    recoverToSucceededIf[IllegalArgumentException] { b.run(action) }
  }

  it should "throw a NoSuchElement exception when the result in action.failed does not contain an exception" in {
    recoverToSucceededIf[NoSuchElementException] { b.run(Action.successful(10).failed) }
  }

  it should "return a future value" in {
    val f = Future(1)
    val action = Action.from(f)
    val result = b.run(action)
    result.map { r =>
      r should be(1)
    }
  }

  it should "map" in {
    val action = Action.successful(1).map(_ + 1)
    val result = b.run(action)
    result.map { r =>
      r should be(2)
    }
  }

  it should "flatMap" in {
    val action = Action.successful(1)
    val action2 = action.flatMap(n => Action.successful(n + 1))
    val result = b.run(action2)
    result.map { r =>
      r should be(2)
    }
  }

  it should "sequence actions, run them, return Unit" in {
    val r = new AtomicInteger(0)
    val actions = (1 to 10).map(n => ContextualAction {
      r.incrementAndGet()
      n
    })
    val result = b.run(Action.seq(actions: _*))
    result.map { res =>
      res should be(())
    }
    r.get should be(10)

    val actions2 = (1 to 10).map(n =>
      ContextualAction[Int, MyTestBackend] { ctx: MyTestBackend#Context =>
        r.incrementAndGet()
        n
      })
    val action2 = Action.seq(actions2: _*)
    val result2 = b.run(action2)
    result2.map { res =>
      res should be(())
      r.get should be(20)
    }
  }

  it should "allow easy extraction to a value" in {
    val x = Action.successful(Some(10))
    val y = x.map(_.get)
    val r = b.run(y)
    r.map { res =>
      res should be(10)
    }

    val extracted = x.flatMap { opt =>
      opt match {
        case Some(x) => Action.successful(x)
        case _ => Action.failed(new NoSuchElementException("no el"))
      }
    }
    b.run(extracted).map { res =>
      res should equal(10)
    }
  }

  it should "project using action.failed correctly" in {
    val x = Action.failed(new IllegalArgumentException("blah")).failed
    b.run(x).map { ex =>
      ex.getMessage should equal("blah")
    }
  }

  it should "project using action.failed correctly when there is no exception thrown" in {
    val x = Action.successful(10).failed
    recoverToSucceededIf[NoSuchElementException] {
      b.run(x)
    }
  }

  it should "always call finally if action was successful anyway" in {
    val counter = new AtomicInteger(0)

    val af = Action.successful(10).andFinally(Action.lift {
      counter.incrementAndGet()
    })
    b.run(af).map { res =>
      counter.get should equal(1)
      res should equal(10)
    }
  }
  it should "always call finally if action was failed" in {
    val counter = new AtomicInteger(0)

    val a2 = Action.failed(new IllegalArgumentException("blah"))
    val af2 = a2.andFinally(ContextualAction {
      counter.incrementAndGet()
    })
    recoverToSucceededIf[IllegalArgumentException] {
      val f = b.run(af2)
      counter.get should equal(1)
      f
    }
  }

  it should "propagate base action's failure when using finally" in {
    val counter = new AtomicInteger(0)

    val a: IOAction[Int] = Action.failed(new IllegalArgumentException("blah"))
    val af = a.andFinally(ContextualAction {
      counter.incrementAndGet()
    })
    val f = b.run(af)
    recoverToSucceededIf[IllegalArgumentException] {
      counter.get should be(1)
      f
    }
  }

  it should "call cleanup with None when successful" in {
    val counter = new AtomicInteger(0)

    val x = Action.successful(Some(10))
    val y = x.map(_.get)

    val cleanup = y.cleanUp { err: Option[Throwable] =>
      err match {
        case Some(t) =>
          fail("base action was successful so this should not be called")
        case _ =>
          counter.incrementAndGet() // should run
          Action.successful(-1) // should never see it!
      }
    }
    b.run(cleanup).map { res =>
      res should be(10)
      counter.get should be(1)
    }
  }

  it should "call cleanup with Some(ex) when failure occurs" in {
    val counter = new AtomicInteger(0)
    // Acually throw an exception, could use Action.failed to create this...
    val x = Action.lift { throw new IllegalArgumentException("blah") }
    val cleanup = x.cleanUp { err: Option[Throwable] =>
      err match {
        case Some(t) =>
          counter.incrementAndGet() // should run
          Action.successful(10)
        case None =>
          fail("base action was failed so this should not be called")
      }
    }
    // Small track, you need the projection, otherwise the exception would be thrown during run!
    b.run(cleanup.failed).map { ex =>
      counter.get should be(1)
    }
  }

  it should "run the cleanup action even if base action was successful" in {
    val counter = new AtomicInteger(0)
    val cleanup = Action.successful(10).cleanUp { err: Option[Throwable] =>
      err match {
        case Some(t) =>
          fail("base action was successful so this should not be called")
        case None =>
          Action.lift(counter.incrementAndGet)
      }
    }
    b.run(cleanup).map { res =>
      counter.get should be(1)
    }
  }

  it should "run the cleanup action even if base action fails" in {
    val counter = new AtomicInteger(0)
    val cleanup = Action.lift{ throw new IllegalArgumentException("blah")}.cleanUp { err: Option[Throwable] =>
      err match {
        case Some(t) =>
          Action.lift(counter.incrementAndGet)
        case None =>
          fail("base action was successful so this should not be called")
      }
    }
    b.run(cleanup.failed).map { res =>
      counter.get should be(1)
    }
  }

  it should "pass exception from base action to cleanup when keepFailure=true" in {
    // slight change in style, we actually throw the exception instead of using Action.failed
    val baseAction: IOAction[Int] = Action.lift { throw new IllegalArgumentException("ouch!") }
    val cleanup: IOAction[Int] = baseAction
      .cleanUp { err: Option[Throwable] =>
        err match {
          case Some(t) =>
            Action.failed(new NoSuchElementException())
          case _ =>
            fail("base action was failed so this should not be called")
        }
      }
    recoverToSucceededIf[IllegalArgumentException] {
      b.run(cleanup)
    }
  }

  it should "return cleanup action's exception if base fails, cleanUp fails and keepFailure=false" in {
    // lazy many style to create a failed action
    val baseAction: IOAction[Int] = Action.failed(new IllegalArgumentException("ouch!"))
    val cleanup: IOAction[Int] = baseAction.cleanUp({
      _ match {
        case Some(t) => Action.failed(new NoSuchElementException())
        case _ => fail("base action was failed so this should not be called")
      }
    }, false)
    recoverToSucceededIf[NoSuchElementException] { b.run(cleanup) }
  }

}
