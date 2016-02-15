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

import scala.concurrent.duration._
import scala.scalajs.js._
import scala.scalajs.js.JSApp
import scala.scalajs.js.timers._
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import annotation.JSExportDescendentObjects
import scala.scalajs.js.UndefOr.any2undefOrA
import scala.scalajs.js.UndefOr.undefOr2ops
import scala.language._

import backend.dom.DOMBackend._;
import SVGAttributes._
import UIEvents._
import VNode._
import HTML5Attributes._
import Styles._
import events._
import Handler.Implicits._

import _root_.org.scalajs.{ dom => d }
import d.document

object Test extends JSApp {

  def main(): Unit = {
    println("test of scala-vdom library")

    val vdom = document.getElementById("vdomtest")
    if (vdom != null)
      vdomTest()

    val delegate = document.getElementById("delegatetest")
    if (delegate != null)
      delegateTest()

    val todo = document.getElementById("todo")
    if (todo != null)
      todotest(todo)

  }

  var _idcounter: Int = -1
  def idcounter = {
    _idcounter += 1
    _idcounter
  }
  case class ToDo(id: Int, content: String)
  val todoDb: collection.mutable.ListBuffer[ToDo] = collection.mutable.ListBuffer()
  var todoNode: VNode = empty

  /** Render todo vnode */
  def renderTodo(root: d.Node): VNode = {
    tag("div",
      tag("input", Seq(keyup ~~> { (e: d.Event, t: d.EventTarget) =>
        {
          val kevent = e.asInstanceOf[d.KeyboardEvent]
          if (kevent.keyCode == d.ext.KeyCode.Enter) {
            println(s"$e, $t")
            val todoText = t.asInstanceOf[d.raw.HTMLInputElement].value
            if (todoText.length > 0) {
              todoDb += ToDo(idcounter, todoText)
              val previousTodoNode = todoNode
              todoNode = renderTodo(root)
              println("rendering new list")
              println(s"previous: $previousTodoNode\nnew: $todoNode")
              val p = diff(previousTodoNode, todoNode)
              //println(s"$p")
              val ioaction: IOAction[_] = p(root)
              //println(s"$ioaction\n")
              run(ioaction)
            }
          }
        }
        true
      })),
      tag("ul",
        todoDb.map(i =>
          tag("li", text(i.content))): _*))
  }

  /**
   *  Ubiquitous TODO application.
   */
  def todotest(root: d.Node) = {
    println("todo test")

    Seq(ToDo(idcounter, "Get up in the morning"), ToDo(idcounter, "Eat breakfast")).foreach(todoDb += _)
    todoNode = renderTodo(root)
    run(diff(empty, todoNode)(root))
  }

  def delegateTest() = {
    println("starting delegate test")

    val test1: UndefOr[d.Element] = document.getElementById("test1target")
    test1.foreach { el =>
      println("attaching test1target click handler")
      var test1counter = 0
      val del = Delegate()
      // add a handler
      del.on("click", (event, node) =>
        {
          println(s"test1target clicked $test1counter")
          test1counter += 1
          true
        })
      // attach it
      del.root(Some(el))
    }

    val test2: UndefOr[d.Element] = document.getElementById("test2target")
    var test2counter = 0
    test2.foreach { el =>
      println("attaching test2target mouse over handler")
      val del = Delegate(Some(el))
      del.on("mouseover", (event, node) => {
        println(s"test2target mouseover $test2counter")
        test2counter += 1
        true
      })
    }

    val test3a: UndefOr[d.Element] = document.getElementById("test3targeta")
    val test3b: UndefOr[d.Element] = document.getElementById("test3targetb")
    test3a.foreach { el =>
      println("attaching test3target resetting root")
      var test3counter = 0
      val del = Delegate()
      del.on("click", (event, node) => {
        println(s"test3target clicked $test3counter")
        test3counter += 1
        true
      })
      del.root(Some(el))
      del.root(None)
      test3b.foreach { e => del.root(Some(e)) }

    }

    val test4child: UndefOr[d.Element] = document.getElementById("test4child")
    val test4parent: UndefOr[d.Element] = document.getElementById("test4parent")
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

    val test5target: UndefOr[d.Element] = document.getElementById("test5target")
    val test5input: UndefOr[d.Element] = document.getElementById("test5input")
    test5target.foreach { el =>
      require(test5input != null)

      val vnodeB = tag("div", tag("p", text("Clicked!")))
      val vnodeA = tag("div", tag("button",
        Seq(click ~~> Handler { (e: d.Event) =>
          {
            // Create a replacement for this node when clicked, should force
            // cleanup actions to be called.
            val node = e.target
            test5input.foreach { pnode =>
              // cheat, run a remove patch for convenience
              run(RemovePatch.applyTo(Seq(0))(pnode))
              // then just add this by hand not using patch, just to confuse you!
              run(InsertPatch(vnodeB)(pnode))
            }
            true
          }
        }), text("Click Me!")))

      // Create vnodeA and append it to test5input
      // Add an artificial cleanup action to test cleanup actions being attached and called. 
      test5input.foreach { pnode =>
        val newNode = run(InsertPatch(vnodeA)(pnode))
        newNode.foreach(addDetachAction(_, Action.lift { println("Cleanup occurred!") }))
      }
    }

  }

  def vdomTest() = {
    println("vdom test")

    val p1 = TextPatch("test1 succeeded: add text to existing element by replacing text")
    val target = document.getElementById("test1")
    require(target != null)
    run(p1(target))

    val p1_5 = TextPatch("test1.5 succeeded: add text to existing text element adding it directly")
    val target1_5 = document.getElementById("test1_5")
    require(target1_5 != null)
    run(p1_5(target1_5))

    val target2 = document.getElementById("test2")
    val p2 = InsertPatch(tag("p", text("test2 succeeded: a new child was inserted")))
    run(p2(target2))

    val target3 = document.getElementById("test3")
    val p3 = ReplacePatch(tag("div", tag("p", text("test3 succeeded: replace a child"))))
    run(p3(target3))

    val target4 = document.getElementById("test4")
    val p4 = InsertPatch(tag("div", Seq(cls := Some("surroundme2")),
      tag("div",
        tag("p", text("line 1")),
        tag("p", text("line 2")))))
    run(p4(target4))

    val target5 = document.getElementById("test5")
    val vdom5_a = tag("div", Seq(cls := Some("surroundme2")),
      tag("p", text("test 5 - if you see this the test failed")),
      tag("p", text("test 5 - if you see the test failed")))
    val vdom5_b = tag("div", Seq(cls := Some("surroundme2")),
      tag("p", text("success! test 5 new line 1")),
      tag("p", text("success! test 5 new line 2")))

    // Diff vdom5_a with the empty node, this should create an insert patch          
    val patch5 = diff(empty, vdom5_a)

    val result5 = for {
      newChild <- run(patch5(target5))
      patch5_1 = diff(vdom5_a, vdom5_b)
      newChild2 <- run(patch5_1(newChild))
    } yield newChild2

    // Test patching via a one level path
    val target5aa = document.getElementById("test5a")
    val vdom5aa = tag("div", Seq(cls := Some("surroundme")), text("test5a success on path!"))
    val patch5aa = PathPatch(InsertPatch(vdom5aa), Nil)
    run(patch5aa(target5aa))

    // Test patching a child while using PathPatches.
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
    // Wait till the patch has run - we should always run in the "callback"
    // We could also sequence the runs (outputing futures) using a for comprehension
    // new6result now holds the DOM nodes for connected to vdom6b virtual nodes
    val new6result = new6.flatMap { newNode =>
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
    val target8 = document.getElementById("test8")
    val svg8 = VNode.svg(Seq(), tag("rect", None, Some(Constants.NS.SVG),
      Seq(x := "30", width := "40", height := "10",
        Styles.stroke := "#00cc00", Styles.fill := "#006600", width := "100", height := "20")))
    val new8 = run(InsertPatch(svg8)(target8))

    //
    // Test 9 - test Delegate.
    //
    val target9 = document.getElementById("test9button")
    val del = Delegate()
    del.on("click", (e: d.Event, t: d.EventTarget) => {
      println(s"click event capture: $e, $t")
      true
    }).delegate.root(Some(target9))

    //
    // Test 10 - button that has a callback which updates the vdom
    //
    // The flow looks messy because we are operating at the vdom layer
    // instead of a nice react-like layer. Just imagine that the vars are
    // part of a cool, higher-level component.
    //
    val target10 = document.getElementById("test10button")
    var count10: Int = 0
    var button10: Future[d.Node] = Future(null)
    var tree10: VNode = null

    lazy val clickHandler: Handler = (e: d.Event) => {
      count10 += 1
      // re-create the virtual tree
      val newTree = create10(count10, clickHandler)
      // calculate patch with new virtual tree
      val patch = diff(tree10, newTree)
      // apply the patch against the current DOM button and update the real DOM button with the result
      button10 = button10.flatMap(b => run(patch(b)))
      // update the previous virtual tree with the new virtual tree
      tree10 = newTree
      true
    }

    def create10(count: Int, handler: Handler): VNode = tag("div", tag("button",
      // When clicked, re-render...
      Seq(click ~~> handler),
      text(s"Click Me - $count")), text(s"You have clicked the button $count10 times!"))

    // create the initial virtual button
    tree10 = create10(count10, clickHandler)
    // create the DOM button from the virtual button
    button10 = run(render(tree10))
    // add the new DOM button to the DOM tree so it displays
    button10.foreach(target10.appendChild(_))

    val target11 = document.getElementById("test11")
    val data = Seq("one", "two", "three", "four")
    def renderList(data: Seq[String]) = data.map(c => tag("li", text(c)))
    run(InsertPatch(tag("ul", renderList(data): _*))(target11))

    val rendertest1 = document.getElementById("rendertest1")
    val rt1vnode = tag("div",
      tag("p", text("Test creating markup from vnodes - you should see an unnumbered list with 3 items")),
      tag("ul",
        tag("li", tag("i", text("bullet 1"))),
        tag("li", tag("em", text("bullet 2"))),
        tag("li", tag("b", text("bullet 3")))))
    run(InsertPatch(rt1vnode)(rendertest1))

  }

}
