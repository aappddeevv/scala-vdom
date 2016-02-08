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
package backend

/**
 * Adler32 checksum.
 *
 * @see http://patterngazer.blogspot.com/2012/01/naive-adler32-example-in-clojure-and.html
 */
private[backend] trait Adler32 {
  val base = 65521

  def rebased(value: Int) = value % base

  def cumulated(acc: (Int, Int), item: Byte): (Int, Int) = {
    val a = rebased(acc._1 + (item & 0xff))
    (a, (a + acc._2) % base)
  }

  def checksum(data: Traversable[Byte]): Int

  def checksumText(data: Traversable[Char]): Int
}

protected[backend] object DefaultAdler32 extends Adler32 {

  override def checksum(data: Traversable[Byte]): Int = {
    val result = data.foldLeft((1, 0)) { cumulated(_, _) }
    (result._2 << 16) | result._1
  }

  def checksumText(data: Traversable[Char]) = {
    checksum(data.toSeq.map(_.toByte))
  }
}

protected[backend] object Utils {

  /** Attribute name for the checksum when placed in markup. */
  val CHECKSUM_ATTR_NAME = "data-vdom-checksum"

  /**
   * Return Adler32 checksum.
   */
  def adler32(str: String): Int = DefaultAdler32.checksumText(str)

  /** Append two strings */
  def stringAppend(left: String, right: String) = left + right

  /** Filter for filtering attribute lists. Keeps StyleKey attributes. */
  def keepStyles(el: KeyValue[_]) = el match {
    case KeyValue(StyleKey(_), _) => true
    case _ => false
  }

  /** Filter for filtering attribute lists. Keeps AttrKey attributes. */
  def keepAttributes(el: KeyValue[_]) = el match {
    case KeyValue(AttrKey(_, _), _) => true
    case _ => false
  }

  /**
   * Take a value and convert it to a quoted string suitable for use
   * as an attribute's value in markup.
   */
  def quoteValueForBrowser[T](v: T): String =
    "\"" + escapeTextForBrowser(v) + "\""

  private[this] val escapeTable = Seq(
    ("&".r, "&amp;"),
    (">".r, "&gt;"),
    ("<".r, "&lt;"),
    ("\"".r, "&quot;"),
    ("'".r, "&#x27;"))

  /**
   * Escape certain char sequences to avoid scripting attacks.
   *
   * TODO: Horribly inefficient!
   */
  def escapeTextForBrowser[T](v: T): String = {
    var rval = v.toString
    for (p <- escapeTable)
      rval = p._1.replaceAllIn(rval, p._2)
    rval
  }

  /**
   * Check hint structure and apply business rule about whether this
   * value should be ignored and does not need to be set into a DOM object.
   */
  def ignoreValue[T](key: KeyPart, hint: Option[AttrHint], value: T): Boolean = {
    false
  }

  /**
   * Create markup for a key value pair. It integrates in attribute
   * hints and considers the value. This includes `key=value`.
   *
   * If no hint is found, generate a simple `name = 'value'` and convert the
   * value to a quoted string. If the value is None then it
   * generates a None return value.
   *
   * @return None if no markup was generated or a string of markup.
   */
  def createMarkupForProperty(kv: KeyValue[_]): Option[String] = {
    val hintopt = DOMAttrHints.hint(kv.key.name)
    kv.value.
      filterNot(v => ignoreValue(kv.key, hintopt, v)).
      map { v => kv.key.name + "=" + quoteValueForBrowser(v) }
  }

  /**
   * Take a style value and prepare it to be inserted into markup.
   */
  def quoteStyleValueForBrowser[T](v: T) = v.toString.trim

  /**
   * Create style markup. This does NOT include `style=` or
   * surrounding quotes. None values are not processed.
   *
   * @return None if no markup was generated or a string of markup.
   */
  def createMarkupForStyles(kvs: Seq[KeyValue[_]]): Option[String] = {
    val rval = kvs.filter(_.value.isDefined).map { kv =>
      kv.key.name + ":" + quoteStyleValueForBrowser(kv.value.get)
    }.mkString(";")
    rval match { 
      case "" => None
      case _ => Some(rval +";")
    }
  }

}