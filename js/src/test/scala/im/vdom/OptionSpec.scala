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

import _root_.org.scalajs.{ dom => d }

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.OptionValues
import org.scalatest.prop

/**
 * Test OptionOpps.
 */
class OptionSpec extends FlatSpec
    with Matchers
    with OptionValues
    with prop.TableDrivenPropertyChecks {

  import im.vdom.OptionOps

  val goodwildcard = Table(
    ("lhs", "rhs"),
    (None, None),
    (Some(1), None),
    (Some("blah"), None))

  it should "find good wildCardEq" in {
    forAll(goodwildcard) { (lhs: Option[Any], rhs: Option[Any]) =>
      lhs wildcardEq rhs should equal(true)
    }
  }

  val badwildcard = Table(
    ("lhs", "rhs"),
    (None, Some(1)),
    (Some(3), Some(4)))

  it should "find bad wildCardEq" in {
    forAll(badwildcard) { (lhs: Option[Any], rhs: Option[Any]) =>
      lhs wildcardEq rhs should equal(false)
    }
  }

  it should "handle ===" in { 
    Some(3) === Some(3) should equal (true)
    None === None should equal (false)
    None === Some(3) should equal (false)
    Some(4) === Some(5) should equal (false)
  }
  
  
}

