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
import scala.scalajs.js
import scala.scalajs.js.JSApp
import scala.scalajs.js.timers._
import scala.concurrent.{ Future, ExecutionContext }
import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import org.scalajs.dom
import org.scalajs.dom.document
import org.im.vdom.backend.DOMBackend

object Test extends JSApp {

  import org.im.vdom._
  import VNode._
  import HTML5Attributes._
  import Style._
  import DOMBackend._ // brings in the core implicits

  def main(): Unit = {
    println("test of scala-vdom")

    val p1 = TextPatch("test1 succeeded: add text to existing element by replacing text")
    val target = document.getElementById("test1")
    require(target != null)
    run(p1(target))

    val p1_5 = TextPatch("test1.5 succeeded: add text to existing text element adding it directly")
    val target1_5 = document.getElementById("test1_5")
    require(target1_5 != null)
    run(p1_5(target1_5))

    val target2 = document.getElementById("test2")
    require(target2 != null)
    val p2 = InsertPatch(vnode("p", vnode("test2 succeeded: insert a new child")))
    run(p2(target2))

    val target3 = document.getElementById("test3")
    val p3 = ReplacePatch(vnode("div", vnode("p", vnode("test3 succeeded: replace a child"))))
    run(p3(target3))

    val target4 = document.getElementById("test4")
    val p4 = InsertPatch(vnode("div", Seq(cls := Some("surroundme2")),
      vnode("div", vnode("p", vnode("line 1")), vnode("p", vnode("line2")))))
    run(p4(target4))

    val target5 = document.getElementById("test5")
    val vdom5_a = vnode("div", Seq(cls := Some("surroundme2")),
      vnode("p", Seq(), vnode("test 5 word 1 - original"), vnode("test 5 word 2 - original")))
    val vdom5_b = vnode("div", Seq(cls := Some("surroundme2")),
      vnode("p", vnode("success! test 5 new line 1")), vnode("p", vnode("success! test 5 new line 2")))

    val patch5 = diff(empty, vdom5_a)
    run(patch5(target5)).foreach { n =>
      val patch5_1 = diff(vdom5_a, vdom5_b)
      run(patch5_1(n))
    }

    // Test patching via a one level path
    val target5aa = document.getElementById("test5a")
    val vdom5aa = vnode("div", Seq(cls := Some("surroundme")), vnode("testa success on path!"))
    val patch5aa = PathPatch(InsertPatch(vdom5aa), Nil)
    run(patch5aa(target5aa))

    // Test patching a child
    val target5ab = document.getElementById("test5b_parent")
    val vdom5ab = vnode("div", Seq(cls := Some("surroundme")), vnode("test5b success on path!"))
    // Adds a path two different ways
    val patch5ab = RemovePatch.applyTo(Seq(0)) andThen PathPatch(InsertPatch(vdom5ab))
    run(patch5ab(target5ab))

    val target6 = document.getElementById("test6")
    val vdom6a = vnode("div", "key1", Seq(id := "willbedropped", cls := "surroundme"),
      vnode("p", vnode("test 6 line 1")),
      vnode("p", vnode("test 6 line 2")))
    val vdom6b = vnode("div", "key1", Seq(cls := Some("surroundme2")),
      vnode("p", vnode("success! test 6 line 1")),
      vnode("span", vnode("***")),
      vnode("p", vnode("success! test 6 line 2")))

    // patch between two vdom trees
    val patch6b = diff(vdom6a, vdom6b)
    // create an IOAction to install the first vdom into the DOM
    val p6 = ReplacePatch(vdom6a)(target6)
    // Run the IOACtion to perform the DOM update
    // new6 holds the new node that was created in the ReplacePatch
    val new6 = run(p6)
    // wait till the patch has run - we should always run in the "callback"
    // we could also sequence the runs (outputing futures) using a for comprehension
    new6.foreach { newNode =>
      // don't worry about the future here, we are done with test6
      val action = patch6b(newNode)
      run(action)
    }

    //
    // Expanding box
    //
    val target7 = document.getElementById("test7")

    def box(count: Int) = {
      val s = Seq(textAlign := "center",
        lineHeight := s"${100 + count * 2}px",
        border := "1px solid red",
        width := s"${100 + 5 * count}px",
        height := s"${100 + 2 * count}px")
      vnode("div", Some("box"), s, vnode(count.toString))
    }

    var count = 0
    var tree = box(count)
    var rootNode = run(render(tree))
    rootNode.foreach(target7.appendChild(_)) // manual append

    val cancel = setInterval(1000) {
      count += 1
      val newTree = box(count)
      val patch = diff(tree, newTree)
      rootNode.foreach(n => run(patch(n)))
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

    //
    // Test 8 - SVG example
    //
    import SVGAttributes._
    val target8 = document.getElementById("test8")
    val svg8 = svg(Seq(), vnode("rect", None, Some(Constants.NS.SVG),
      Seq(x := "30", width := "40", height := "10",
        stroke := "#00cc00", fill := "#006600", width := "100", height := "20")))
    val new8 = run(InsertPatch(svg8)(target8))

    //
    // Test 9 - test Delegate.
    //
    val target9 = document.getElementById("test9button")
    import events._
    var d = Delegate()
    d.on("click", (d: dom.Event, t: dom.EventTarget) => {
      println(s"click event capture: $d, $t")
      true
    }).root(Some(target9))

    import UIEvents._

    //
    // Test 10 - button that has a callback which updates the vdom
    //
    // The flow looks messy because we are operating at the vdom layer
    // instead of a nice react-like layer. Just imagine that the vars are
    // part of a cool, higher-level component.
    //
    val target10 = document.getElementById("test10button")
    var count10: Int = 0
    var button10: Future[dom.Node] = Future(null)
    var tree10: VNode = null

    def create10(count: Int): VNode = vnode("div", vnode("button",
      // When clicked, re-render...
      Seq(click ~~> ((d: dom.Event) => {
        count10 += 1
        // re-create the virtual tree
        val newTree = create10(count10)
        // calculate patch with new virtual tree
        val patch = diff(tree10, newTree)
        // apply the patch against the current DOM button and update the real DOM button with the result
        button10 = button10.flatMap(b => run(patch(b)))
        // update the previous virtual tree with the new virtual tree
        tree10 = newTree
        true
      })),
      text(s"Click Me - $count")), text(s"You have clicked the button $count10 times!"))

    // create the initial virtual button
    tree10 = create10(count10)
    // create the DOM button from the virtual button
    button10 = run(render(tree10))
    // add the new DOM button to the DOM tree so it displays
    button10.foreach(target10.appendChild(_))
  }
}
