package im.vdom

import org.scalatest.WordSpec
import ScalaXml._
import VNode._

class ScalaXmlSpec extends WordSpec {
  "ScalaXml.nodeToVNode()" should {
    "convert a trivial empty Node into a VNode.tag" in {
      val html = <br/>
      val expected = tag("br")

      assertResult(expected)(nodeToVNode(html))
    }

    "convert an empty Node with attributes" in {
      val html = <div id="my-id" class="my-class"></div>
      val expected = tag("div", Seq(KeyValue(AttrKey("id"), Some("my-id")), KeyValue(AttrKey("class"), Some("my-class"))))

      assertResult(expected)(nodeToVNode(html))
    }

    "convert a Node with text" in {
      val html = <span>This is text</span>
      val expected = tag("span", text("This is text"))

      assertResult(expected)(nodeToVNode(html))
    }

    "convert a Node with children and attributes" in {
      val html =
        <form method="post">
          <div>
            <input type="text" id="chat-in" name="in"/>
            <input type="submit" value="Submit"/>
          </div>
        </form>

      val expected =
        tag("form", Seq(KeyValue(AttrKey("method"), Some("post"))),
          tag("div",
            tag("input", Seq(KeyValue(AttrKey("type"), Some("text")), KeyValue(AttrKey("id"), Some("chat-in")), KeyValue(AttrKey("name"), Some("in")))),
            tag("input", Seq(KeyValue(AttrKey("type"), Some("submit")), KeyValue(AttrKey("value"), Some("Submit"))))
          )
        )

      assertResult(expected)(nodeToVNode(html))
    }
  }

}
