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

import scala.concurrent.duration._
import scala.concurrent.Await
import scala.scalajs.js.JSApp
import scala.scalajs.js.timers._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import org.scalajs.dom
import org.scalajs.dom.document
import org.im.vdom.backend.DOMBackend

object Test extends JSApp {

  import org.im.vdom._
  import VNode._
  import HTML5Attributes._
  import Style._
  import DOMBackend._ // brings in alot of implicits


  def main(): Unit = {
    println("test of scala-vdom")

    val p1 = TextPatch("test1 succeeded: add text to existing element by replacing text")
    val target = document.getElementById("test1")
    require(target != null)
    DOMBackend.run(p1(target))

    val p1_5 = TextPatch("test1.5 succeeded: add text to existing text element adding it directly")
    val target1_5 = document.getElementById("test1_5")
    require(target1_5 != null)
    DOMBackend.run(p1_5(target1_5))

    val target2 = document.getElementById("test2")
    require(target2 != null)
    val p2 = InsertPatch(vnode("p", vnode("test2 succeeded: insert a new child")))
    DOMBackend.run(p2(target2))

    val target3 = document.getElementById("test3")
    val p3 = ReplacePatch(vnode("div", vnode("p", vnode("test3 succeeded: replace a child"))))
    DOMBackend.run(p3(target3))

    val target4 = document.getElementById("test4")
    val p4 = InsertPatch(vnode("div", Seq(cls := Some("surroundme2")),
      vnode("div", vnode("p", vnode("line 1")), vnode("p", vnode("line2")))))
    DOMBackend.run(p4(target4))

    val target5 = document.getElementById("test5")
    val vdom5_a = vnode("div", Seq(cls := Some("surroundme2")),
      vnode("p", Seq(), vnode("test 5 word 1 - original"), vnode("test 5 word 2 - original")))
    val vdom5_b = vnode("div", Seq(cls := Some("surroundme2")),
      vnode("p", vnode("success! test 5 new line 1")), vnode("p", vnode("success! test 5 new line 2")))

    val patch5 = diff(empty, vdom5_a)
    DOMBackend.run(patch5(target5)).foreach { n =>
      val patch5_1 = diff(vdom5_a, vdom5_b)
      DOMBackend.run(patch5_1(n))
    }

    // Test patching via a one level path
    val target5aa = document.getElementById("test5a")
    val vdom5aa = vnode("div", Seq(cls := Some("surroundme")), vnode("testa success on path!"))
    val patch5aa = PathPatch(InsertPatch(vdom5aa), Nil)
    DOMBackend.run(patch5aa(target5aa))

    val target5ab = document.getElementById("test5b_parent")
    val vdom5ab = vnode("div", Seq(cls := Some("surroundme")), vnode("test5b success on path!"))
    val patch5ab = RemovePatch().applyTo(Seq(0)) andThen PathPatch(InsertPatch(vdom5ab))
    DOMBackend.run(patch5ab(target5ab))

    val target6 = document.getElementById("test6")
    val vdom6a = vnode("div", "key1", Seq(id := "willbedropped", cls := "surroundme"),
      vnode("p", vnode("test 6 line 1")),
      vnode("p", vnode("test 6 line 2")))
    val vdom6b = vnode("div", "key1", Seq(cls := Some("surroundme2")),
      vnode("p", vnode("success! test 6 line 1")),
      vnode("span", vnode("***")),
      vnode("p", vnode("success! test 6 line 2")))

    val new6 = DOMBackend.run(ReplacePatch(vdom6a)(target6))
    val patch6b = diff(vdom6a, vdom6b)    
    new6.foreach(n => DOMBackend.run(patch6b(n)))

    //
    // Expanding box
    //
    val target7 = document.getElementById("test7")

    def box(count: Int) = vnode("div", Some("box"),
      Seq(textAlign := "center", lineHeight := s"${100 + count}px",
        border := "1px solid red", width := s"${100 + count}px", height := s"${100 + count}px"),
      vnode(count.toString))

    var count = 0
    var tree = box(count)
    var rootNode = DOMBackend.render(tree)
    rootNode.foreach(target7.appendChild(_)) // manual append

    val cancel = setInterval(1000) {
      count += 1
      val newTree = box(count)
      val patch = diff(tree, newTree)
      rootNode.flatMap { n => DOMBackend.run(patch(n)) }
      tree = newTree
    }
    setTimeout(10 seconds)(clearInterval(cancel))

    //
    // Compare Test7 to the javascript version from virtual-dom.
    //

    // 1: Create a function that declares what the DOM should look like
    //function render(count)  {
    //    return h('div', {
    //        style: {
    //            textAlign: 'center',
    //            lineHeight: (100 + count) + 'px',
    //            border: '1px solid red',
    //            width: (100 + count) + 'px',
    //            height: (100 + count) + 'px'
    //        }
    //    }, [String(count)]);
    //}
    //
    // 2: Initialise the document
    //var count = 0;      // We need some app data. Here we just store a count.
    //
    //var tree = render(count);               // We need an initial tree
    //var rootNode = createElement(tree);     // Create an initial root DOM node ...
    //document.body.appendChild(rootNode);    // ... and it should be in the document
    //

    // 3: Wire up the update logic
    //setInterval(function () {
    //      count++;
    //
    //      var newTree = render(count);
    //      var patches = diff(tree, newTree);
    //      rootNode = patch(rootNode, patches);
    //      tree = newTree;
    //}, 1000);
  }
}
