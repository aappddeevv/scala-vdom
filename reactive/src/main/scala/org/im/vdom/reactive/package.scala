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

import monifu.reactive._

package object reactive {

  import scala.concurrent.duration._
  import scala.concurrent.Await
  import scala.concurrent.{ Future, ExecutionContext }
  import scala.language._
  
  import monifu.reactive._
  import monifu.concurrent._
  import scala.collection.mutable.ArrayBuffer
  
  implicit class EnhancedObservable[T](source: Observable[T]) {
    def slidingWindow(n: Int): Observable[Seq[T]] = source.whileBusyBuffer(OverflowStrategy.Unbounded).buffer(n, 1)
  }

}