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

import org.im.vdom.backend._
import org.im.vdom.VNode._

import org.scalajs.dom
import concurrent._
import scalajs.js
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.scalatest.AsyncFlatSpec
import org.scalatest.Matchers
import org.scalatest.OptionValues

/**
 * Test rendering virtual nodes to DOM nodes..
 */
class RenderSpec extends AsyncFlatSpec
    with Matchers
    with OptionValues {

  import js.DynamicImplicits.truthValue
  import DOMBackend._
  
  "backend rendering" should "create a simple div" in {
    val vnode = tag("div")
    val elf = DOMBackend.run(render(vnode))
    elf.map { e =>
      e.nodeName shouldEqual "DIV"
    }
  }
  
  it should "render a short, simple tree" in {
    val vnode = tag("div", tag("p", text("some text")))
    val elf = DOMBackend.run(render(vnode))
    elf.map { e =>
      // only the <p> should count
      e.childNodes.length shouldEqual 1
    }
  }
  
  it should "create a simple text node" in { 
    val vnode = text("test")
    val elf = DOMBackend.run(render(vnode))
    elf.map { el => 
      el.textContent shouldEqual "test"      
    }
  }
  
  it should "allow ThunkNode call its function when its asked to" in {
    val vnode = ThunkNode(() => {
      tag("div")
    })
    val elf = DOMBackend.run(render(vnode))
    elf.map { el =>
      el.nodeName shouldEqual "DIV"
    }
  }

}
