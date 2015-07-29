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

import monifu.reactive.Observable
package object reactive {

  import scala.concurrent.duration._
  import scala.concurrent.Await
  import scala.concurrent.{ Future, ExecutionContext }
  import scala.language._
  
  import monifu.reactive._
  import monifu.concurrent._
  import scala.collection.mutable.ArrayBuffer
  
  implicit class EnhancedObservable[T](source: Observable[T]) {

    /** Sliding window of size n. */
    def window(n: Int): Observable[Seq[T]] = Observable.create[Seq[T]] { subscriber =>
      implicit val s = subscriber.scheduler
      val observer = subscriber.observer

      val sourceSubscriber = new monifu.reactive.Observer[T] {
        private[this] var buffer = ArrayBuffer.empty[T]

        def onNext(elem: T): Future[Ack] = {
          buffer.append(elem)
          if (buffer.size == n) {
            val oldBuffer = buffer
            buffer = buffer.drop(1)
            observer.onNext(oldBuffer)
          }
          Ack.Continue
        }

        def onError(ex: Throwable): Unit = observer.onError(ex)

        def onComplete(): Unit = {
          if (buffer.size > 0) {
            observer.onNext(buffer)
            buffer = null
          }
          observer.onComplete()
        }
      }

      source.subscribe(sourceSubscriber)(s)
    }
  }

}