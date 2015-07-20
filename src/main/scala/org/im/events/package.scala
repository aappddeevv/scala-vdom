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

import org.scalajs.dom

package object events {

  /**
   * EventTarget could be a Node or a Window.
   */
  implicit def func2Handler(f: Function2[dom.Event, dom.EventTarget, Boolean]) = Handler { f }

  /**
   * Simple handler that only takes the Event object.
   */
  implicit def func1Hadler(f: Function1[dom.Event, Boolean]) = Handler { f }

}