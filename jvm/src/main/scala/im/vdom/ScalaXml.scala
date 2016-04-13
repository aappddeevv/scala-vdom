package im.vdom

import scala.xml.{Text, UnprefixedAttribute, Node}

object ScalaXml {
  def nodeToVNode(xml:Node):VNode = {
    val attrs = xml.attributes.collect { case UnprefixedAttribute(k, Text(v), _) =>
      KeyValue(AttrKey(k), Some(v))
    }.toSeq
    val children = xml.nonEmptyChildren
      .filter(n => n.label != "#PCDATA" || !n.text.trim.isEmpty)
      .map(nodeToVNode)

    if(xml.label == "#PCDATA") VNode.text(xml.text)
    else VNode.tag(xml.label, attrs, children:_*)
  }
}
