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
package im
package vdom
package backend
package dom

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.queue
import scala.scalajs.js
import scala.scalajs.js._
import scala.scalajs.js.UndefOr
import scala.scalajs.js.UndefOr._
import scala.language._

import _root_.org.scalajs.{dom => d}

import Defaults._

/**
 * A base DOM backend that can be extended with specific components as needed.
 * This trait defines the context type.
 */
trait BasicDOMBackend extends Backend with DOMUtils {
  type This = BasicDOMBackend
  type Context = BasicContext

  protected[this] def createContext() = new BasicContext {}
}

/**
 * An example of extending the basic backend with your components.
 */
trait DOMBackend extends BasicDOMBackend 
  with DOMPatchesComponent
  with RenderToDOMComponent 
  with AttributeComponent
  with DOMHints
  with DOMCleanupActions
  with DelegateComponent
  with ActionLists[d.Node]

/**
 * Use this backend when a DOM patching process is desired.
 */
object DOMBackend extends DOMBackend


/**
Note: https://groups.google.com/forum/#!topic/scala-js/qlUJWSQ6ccE

No Brendon is right: `if (this._map)` tests whether `this._map` is falsy.
To test that according to JS semantics, you can use

js.DynamicImplicits.truthValue(self._map.asInstanceOf[js.Dynamic])

which returns false if and only if `self._map` is falsy. This pattern should typically be avoided in Scala code, unless transliterating from JS.

Cheers,
Sébastien

*/
