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
package reactive

import scala.scalajs.js.JSApp
import org.im.vdom._
import vdom.backend.DOMBackend._
import VNode._
import HTML5Attributes._
import Styles._
import UIEvents._
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.concurrent.{ Future, ExecutionContext }
import scala.language._
import scalajs.js
import js.UndefOr
import org.scalajs.dom
import dom._
import monifu._
import monifu.concurrent.Implicits.globalScheduler
import monifu.reactive._
import channels._
import subjects._
import monifu.concurrent._
import monifu.concurrent.cancelables._
import monifu.reactive.Ack.Continue

/**
 * Derived from: https://github.com/staltz/mvi-example.
 */
object Test extends JSApp {

  // Get an element by name.
  def elementById[A <: js.Any](id: String): A =
    document.getElementById(id).asInstanceOf[A]

  // Convert a dom.Element event to an Observable.
  def elementEventObservable(el: dom.Element, eventType: String): Observable[dom.Event] = {
    val ps = PublishSubject[dom.Event]()
    el.addEventListener(eventType, (e: dom.Event) => ps.onNext(e))
    ps
  }

  // Observe stream of VNodes representing view updates
  def renderer(vtree: Observable[VNode], container: dom.Element): BooleanCancelable = {
    container.innerHTML = ""
    var root: Future[dom.Node] = Future(document.createElement("div"))
    root.foreach(container.appendChild(_))
    vtree.
      startWith(empty).
      window(2).
      subscribe(new Observer[Seq[VNode]] {
        def onNext(els: Seq[VNode]): Future[Ack] = {
          val oldTree = els(0)
          val newTree = els(1)
          val patch = diff(oldTree, newTree)
          root = root.flatMap(el => run(patch(el)))
          Ack.Continue
        }
        def onError(ex: Throwable): Unit = println(s"Error rendering: $ex")
        def onComplete() = ()
      })
  }

  /**
   * Convert view click and low-level events to a semantic, user "intent."
   */
  def intent(viewInfo: Observable[dom.Event]) = viewInfo.map(event => "updateCounter")

  /**
   * Convert user "intent" events (like updateCounter) to model state. Expose
   * state as an observable.
   */
  var modelState: Int = 0 // state is kept in a var for simplicity
  def model(intentInfo: Observable[String]) = {
    intentInfo.map { eventContent =>
      modelState += 1
      modelState
    }
  }

  /**
   * Convert model changes to view changes. The VNode observable changes
   * can be used by a rendering engine to render into the DOM. The user
   * then interacts with that view.
   */
  def view(modelInfo: Observable[Int]) = {
    val clicks = PublishSubject[dom.Event]()
    val vtreeObs = modelInfo.map { count =>
      vnode("div", text(s"count: $count "),
        vnode("button", Seq(click ~~> ((e: dom.Event) => {
          clicks.onNext(e)
          true
        })), text("Click Me!")))
    }
    (vtreeObs, clicks)
  }

  def main(): Unit = {
    
    // Dance around circular dependency issues    
    val placeholder = PublishSubject[Int]()
    val channel = SubjectChannel(placeholder)

    val (vtree, clicks) = view(placeholder)

    val Intent = intent(clicks)

    val Model = model(Intent)

    renderer(vtree, elementById[dom.Element]("test1"))

    // Brake circular dependency and start the "cycle" with the initial state value.
    val cancelable = Model.startWith(modelState).subscribe(new monifu.reactive.Observer[Int] {
      def onNext(i: Int) = {
        channel.pushNext(i)
        Ack.Continue
      }
      def onError(ex: Throwable): Unit = {
        println(s"Error in push: $ex")
        channel.pushError(ex)
      }
      def onComplete() = channel.pushComplete()
    })
  }
}