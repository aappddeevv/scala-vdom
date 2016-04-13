package im.vdom

import org.scalatest.WordSpec
import ScalaXml._

import scala.xml.Node

class ScalaXmlSpec extends WordSpec {
  "ScalaXml.nodeSeqToVNode()" should {
    "convert an empty NodeSeq into VNode.empty" in {
      assertResult(VNode.empty)(nodeToVNode(<empty/>))
    }
  }

}
