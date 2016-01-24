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
import scala.scalajs.js._
import scala.scalajs.js.JSApp
import scala.scalajs.js.timers._
import scala.concurrent.{ Future, ExecutionContext }
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import org.scalajs.dom
import dom._
import org.scalajs.dom.document
import org.im.vdom.backend.DOMBackend

object Test extends JSApp {

  import org.im.vdom._
  import VNode._
  import HTML5Attributes._
  import Styles._
  import DOMBackend._ // brings in the core implicits
  import org.im.events._
  import events.Handler
  import Handler.Implicits._

  def main(): Unit = {
    println("test of scala-vdom")

    val vdom = document.getElementById("vdomtest")
    if (vdom != null)
      vdomTest()

    val delegate = document.getElementById("delegatetest")
    if (delegate != null)
      delegateTest()

  }

  def delegateTest() = {
    println("starting delegate test")

    val test1: UndefOr[Element] = document.getElementById("test1target")
    test1.foreach { el =>
      println("attaching test1target click handler")
      var test1counter = 0
      val d = Delegate()
      // add a handler
      d.on("click", (event, node) =>
        {
          println(s"test1target clicked $test1counter")
          test1counter += 1
          true
        })
      // attach it
      d.root(Some(el))
    }

    val test2: UndefOr[Element] = document.getElementById("test2target")
    var test2counter = 0
    test2.foreach { el =>
      println("attaching test2target mouse over handler")
      val d = Delegate(Some(el))
      d.on("mouseover", (event, node) => {
        println(s"test2target mouseover $test2counter")
        test2counter += 1
        true
      })
    }

    val test3a: UndefOr[Element] = document.getElementById("test3targeta")
    val test3b: UndefOr[Element] = document.getElementById("test3targetb")
    test3a.foreach { el =>
      println("attaching test3target resetting root")
      var test3counter = 0
      val d = Delegate()
      d.on("click", (event, node) => {
        println(s"test3target clicked $test3counter")
        test3counter += 1
        true
      })
      d.root(Some(el))
      d.root(None)
      test3b.foreach { e => d.root(Some(e)) }

    }

    val test4child: UndefOr[Element] = document.getElementById("test4child")
    val test4parent: UndefOr[Element] = document.getElementById("test4parent")
    test4child.foreach { el =>
      require(test4parent.isDefined)
      var test4counter = 0
      println("attaching test4target - upward bubbling but with child id matcher")
      val parentd = new Delegate()
      parentd.on("mouseover", (event, node) => {
        println(s"event fired on test4parent due to match on child id $test4counter")
        test4counter += 1
        false
      }, Matcher.MatchId("test4child"))
      test4parent.foreach { p => parentd.root(Some(p)) }
    }

  }

  def vdomTest() = {
    val p1 = TextPatch("test1 succeeded: add text to existing element by replacing text")
    val target = document.getElementById("test1")
    require(target != null)
    run(p1(target))

    val p1_5 = TextPatch("test1.5 succeeded: add text to existing text element adding it directly")
    val target1_5 = document.getElementById("test1_5")
    require(target1_5 != null)
    run(p1_5(target1_5))

    val target2 = document.getElementById("test2")
    val p2 = InsertPatch(tag("p", text("test2 succeeded: insert a new child")))
    run(p2(target2))

    val target3 = document.getElementById("test3")
    val p3 = ReplacePatch(tag("div", tag("p", text("test3 succeeded: replace a child"))))
    run(p3(target3))

    val target4 = document.getElementById("test4")
    val p4 = InsertPatch(tag("div", Seq(cls := Some("surroundme2")),
      tag("div", tag("p", text("line 1")), tag("p", tag("line2")))))
    run(p4(target4))

    val target5 = document.getElementById("test5")
    val vdom5_a = tag("div", Seq(cls := Some("surroundme2")),
      tag("p", Seq(), text("test 5 word 1 - original"), text("test 5 word 2 - original")))
    val vdom5_b = tag("div", Seq(cls := Some("surroundme2")),
      tag("p", text("success! test 5 new line 1")), tag("p", text("success! test 5 new line 2")))

    val patch5 = diff(empty, vdom5_a)
    run(patch5(target5)).foreach { n =>
      val patch5_1 = diff(vdom5_a, vdom5_b)
      run(patch5_1(n))
    }

    // Test patching via a one level path
    val target5aa = document.getElementById("test5a")
    val vdom5aa = tag("div", Seq(cls := Some("surroundme")), text("testa success on path!"))
    val patch5aa = PathPatch(InsertPatch(vdom5aa), Nil)
    run(patch5aa(target5aa))

    // Test patching a child
    val target5ab = document.getElementById("test5b_parent")
    val vdom5ab = tag("div", Seq(cls := Some("surroundme")), text("test5b success on path!"))
    // Adds a path two different ways
    val patch5ab = RemovePatch.applyTo(Seq(0)) andThen PathPatch(InsertPatch(vdom5ab))
    run(patch5ab(target5ab))

    val target6 = document.getElementById("test6")
    val vdom6a = tag("div", "key1", Seq(id := "willbedropped", cls := "surroundme"),
      tag("p", text("test 6 line 1")),
      tag("p", text("test 6 line 2")))
    val vdom6b = tag("div", "key1", Seq(cls := Some("surroundme2")),
      tag("p", text("success! test 6 line 1")),
      tag("span", text("***")),
      tag("p", text("success! test 6 line 2")))

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
      tag("div", Some("box"), None, s, text(count.toString))
    }

    var count = 0
    var tree = box(count)
    var rootNode = run(render(tree))
    rootNode.foreach(target7.appendChild(_)) // manual append

    val cancel = timers.setInterval(1000) {
      count += 1
      val newTree = box(count)
      val patch = diff(tree, newTree)
      rootNode.foreach(n => run(patch(n)))
      tree = newTree
    }
    timers.setTimeout(10 seconds)(timers.clearInterval(cancel))

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
    val svg8 = svg(Seq(), tag("rect", None, Some(Constants.NS.SVG),
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
    }).delegate.root(Some(target9))

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

    def create10(count: Int): VNode = tag("div", tag("button",
      // When clicked, re-render...
      Seq(click ~~> Handler { (d: dom.Event) =>
        {
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
        }
      }),
      text(s"Click Me - $count")), text(s"You have clicked the button $count10 times!"))

    // create the initial virtual button
    tree10 = create10(count10)
    // create the DOM button from the virtual button
    button10 = run(render(tree10))
    // add the new DOM button to the DOM tree so it displays
    button10.foreach(target10.appendChild(_))
  }
}
