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
import scala.scalajs.js.JSApp
import scala.scalajs.js.timers._

import org.scalajs.dom
import org.scalajs.dom.document

object Test extends JSApp {

  import org.im.vdom._
  import PatchModule._
  import DiffModule._
  import HTML5Attributes._
  import Style._

  val bs = scala.collection.mutable.BitSet()
  bs += 10
  bs.contains(12)

  def main(): Unit = {
    println("test of scala-vdom")

    val p1 = TextPatch("test1 succeeded: add text to existing element by replacing text")
    val target = document.getElementById("test1")
    require(target != null)
    p1(target).run

    val p1_5 = TextPatch("test1.5 succeeded: add text to existing text element adding it directly")
    val target1_5 = document.getElementById("test1_5")
    require(target1_5 != null)
    p1_5(target1_5).run

    val target2 = document.getElementById("test2")
    require(target2 != null)
    val p2 = InsertPatch(VNode("p", VNode("test2 succeeded: insert a new child")))
    p2(target2).run

    val target3 = document.getElementById("test3")
    val p3 = ReplacePatch(VNode("div", VNode("p", VNode("test3 succeeded: replace a child"))))
    p3(target3).run

    val target4 = document.getElementById("test4")
    val p4 = InsertPatch(VNode("div", Seq(cls := Some("surroundme2")),
      VNode("p", VNode("line 1"), VNode("line2"))))
    p4(target4).run

    val target5 = document.getElementById("test5")
    val vdom5_a = VNode("div", Seq(cls := Some("surroundme2")),
      VNode("p", Seq(), VNode("test 5 word 1"), VNode("test 5 word 2")))
    val vdom5_b = VNode("div", Seq(cls := Some("surroundme2")),
      VNode("p", Seq(), VNode("success! test 5 new word 1"), VNode("success! test 5 new word 2")))

    val patch5 = diff(VNode.empty, vdom5_a)
    val new5 = patch5(target5).run
    val patch5_1 = diff(vdom5_a, vdom5_b)
    patch5_1(new5.asInstanceOf[dom.Element]).run

    // Test patching via a one level path
    val target5aa = document.getElementById("test5a")
    val vdom5aa = VNode("div", Seq(cls := Some("surroundme")), VNode("testa success on path!"))
    val patch5aa = PathPatch(InsertPatch(vdom5aa), Nil)
    patch5aa(target5aa).run

    val target5ab = document.getElementById("test5b_parent")
    val vdom5ab = VNode("div", Seq(cls := Some("surroundme")), VNode("test5b success on path!"))
    val patch5ab = RemovePatch().applyTo(Seq(0)) >> PathPatch(InsertPatch(vdom5ab), Seq(0))
    patch5ab(target5ab).run

    val target6 = document.getElementById("test6")
    val vdom6a = VNode("div", "key1", Seq(id := "willbedropped", cls := "surroundme"),
      VNode("div", VNode("test 6 word1")),
      VNode("div", VNode("test 6 word2")))
    val vdom6b = VNode("div", "key1", Seq(cls := Some("surroundme2")),
      VNode("span", VNode("success! test 6 word1")),
      VNode("span", VNode(" - ")),
      VNode("span", VNode("success! test 6 word2")))

    val new6 = ReplacePatch(vdom6a)(target6).run
    val patch6b = diff(vdom6a, vdom6b)
    new6.foreach(n => patch6b(n.asInstanceOf[dom.Element]).run)

    //
    // Expanding box
    //
    val target7 = document.getElementById("test7")

    def render(count: Int) = VNode("div", Some("box"),
      Seq(textAlign := "center", lineHeight := s"${100 + count}px",
        border := "1px solid red", width := s"${100 + count}px", height := s"${100 + count}px"),
      VNode(count.toString))

    var count = 0
    var tree = render(count)
    var rootNode = tree.render
    rootNode.foreach(target7.appendChild(_))

    val cancel = setInterval(1000) {
      count += 1
      val newTree = render(count)
      val patch = diff(tree, newTree)
      rootNode.flatMap { n => patch(n).run }
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
