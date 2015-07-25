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

import org.scalatest._

/**
 * Patches do not need much testing...so this is really
 * just a dummy test class.
 */
class PatchSpec extends FlatSpec
    with Assertions
    with Matchers
    with OptionValues {

  "Patch" should "have only one EmptyPatch" in {
    assertResult(EmptyPatch)(EmptyPatch)
  }
  
  it should "not need arguments for RemovePatch" in {
    assert(RemovePatch != null)
  }

  "andThen" should "sequence left then right" in {
    val lhs = EmptyPatch
    val rhs = RemovePatch
    val andThen = lhs andThen rhs
    assertResult(rhs)(andThen.right)
    assertResult(lhs)(andThen.left)
  }
}