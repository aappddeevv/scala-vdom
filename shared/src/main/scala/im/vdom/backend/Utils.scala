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

protected[backend] object Adler32 extends Adler32 {

  override def checksum(data: Traversable[Byte]): Int = {
    val result = data.foldLeft((1, 0)) { cumulated(_, _) }
    (result._2 << 16) | result._1
  }

  def checksumText(data: Traversable[Char]) = {
    checksum(data.toSeq.map(_.toByte))
  }
}

protected[backend] trait Utils {

  /**
   * Return Adler32 checksum.
   */
  def adler32(str: String): Int = Adler32.checksumText(str)

  /**
   * Name of the custom data attribute that stores a checksum.
   */
  val ChecksumAttrName = "data-scala-vdom-checksum"

  /**
   * Add checksum to end of markup using complete adhoc string regex
   * to find the end tag. Assumes the first set of characters
   * is the start of an element and the tag ends in ">" or "/>".
   */
  def addChecksumToMarkup(markup: String): String = {
    val checksum = adler32(markup)
    markup.replaceFirst("(/?>)", " " + ChecksumAttrName + "=\"" + checksum + "\"$1")
  }

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
  def ignoreValue[T](key: KeyPart, hintopt: Option[AttrHint], value: T): Boolean = {
    (hintopt.map(_.values).getOrElse(Hints.EmptyHints), value) match {
      case (hints, false) if (hints(Hints.HasBooleanValue)) => true
      case _ => false
    }
  }

  /**
   * Create markup for a key value pair. It considers both the hint and the value
   * when generating markup.
   *
   * If no hint is found, generate a simple `name = 'value'` and convert the
   * value to a quoted string. If the value is None then it
   * generates a None return value.
   *
   * @return None if no markup was generated or a string of markup.
   */
  def createMarkupForProperty(kv: KeyValue[_], hintopt: Option[AttrHint]): Option[String] = {
    kv.value.
      filterNot(ignoreValue(kv.key, hintopt, _)).
      map { v =>
        (hintopt.map(_.values).getOrElse(Hints.EmptyHints), v) match {
          case (hints, true) if (hints(Hints.HasBooleanValue) || hints(Hints.HasOverloadedBooleanValue)) =>
            kv.key.name + """="""""
          case _ => kv.key.name + "=" + quoteValueForBrowser(v)
        }
      }
  }

  /**
   * Take a style value and prepare it to be inserted into markup.
   */
  def quoteStyleValueForBrowser[T](hintopt: Option[StyleHint], v: T) = {
    (hintopt.map(_.values).getOrElse(collection.BitSet.empty), v) match {
      case (_, null) => ""
      case (_, true) => ""
      case (_, false) => ""
      case (_, "") => ""
      case (hints, x@_) if (hints(Hints.Unitless)) => v.toString
      case _ => v.toString.trim + "px"
    }
  }

  /** Convert camel cased to a hyphenated name. */
  def hyphenate(name: String) = name.replaceAll("([A-Z])", "-$1").toLowerCase

  /**
   * Process a style name for proper formation. Hyphenate and fix
   * some.
   *
   * For "ms-" prefix convert to "-ms-" per react.
   */
  def processStyleName(name: String) = {
    hyphenate(name).trim.replaceAll("^ms-", "-ms-")
  }

  /**
   * Create style markup. This does NOT include `style=` or
   * surrounding quotes. None values conceptually indicate
   * we should ignore the value so no markup is generated for
   * keys with None values.
   *
   * @return None if no markup was generated or a string of markup.
   *
   * TODO Allow the specification of a hint source.
   */
  def createMarkupForStyles(kv: KeyValue[_], hintopt: Option[StyleHint]): Option[String] =
    kv.value.map { v => processStyleName(kv.key.name) + ":" + quoteStyleValueForBrowser(hintopt, v) }

  /**
   * Norm the string for comparison purposes. Remove repeated whitespace,
   *  remove whitespace before non-alpha characters, remove leading/trailing
   *  whitespace. Note that newlines are stripped as well and cannot
   *  detect correctness if a newline MUST be in the normalized string.
   */
  def norm(input: String): String = {
    input.trim().
      replaceAll("(\\s)+", " ").
      replaceAll("\\s(\\W)", "$1").
      replaceAll("(\\W)\\s", "$1").
      toUpperCase
  }

}

protected[backend] object Utils extends Utils