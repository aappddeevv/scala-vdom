/* Copyright 2015 Devon Miller
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at9
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

import scala.language._
import org.scalatest._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

/**
 * Utils test.
 */
class UtilsSpec extends FlatSpec with Matchers with OptionValues {

  import Utils._
  import collection.immutable.BitSet
import im.vdom.backend.Hints;

  "norm" should "should not change an already normed string" in {
    norm("<div><div>") should equal ("<DIV><DIV>")
  }
  
  it should "norm an empty string to an empty string" in { 
    norm("") should equal("")
  }
  
  it should "remove leading and trailing space" in { 
    norm(" a  \n") should equal ("A")
  }
  
  it should "remove repeated whitespace" in { 
    norm(" a\n b ") should equal ("A B")
  }
  
  it should "norm a trailing space before a non-alpha character" in { 
    norm("a >") should equal ("A>")
  }

  it should "norm a crazy div" in { 
    norm(" < div  \n></div  >  \n") should equal ("<DIV></DIV>")
  }
  
  it should "norm text node correctly" in { 
    norm("<p> this is \na text block</p>") should equal ("<P>THIS IS A TEXT BLOCK</P>")
  }
  
  "hyphenate" should "convert camel cased to hyphens" in { 
    hyphenate("blahHah") should equal ("blah-hah")
  }
  
  it should "work on non camel cased names" in { 
    hyphenate("blah") should equal ("blah")
  }
  
  it should "remove leading and trailing spaces" in { 
    hyphenate("blahHah") should equal ("blah-hah") 
  }
  
  "processStyleName" should "camel case the name" in { 
    processStyleName("blahHah") should equal ("blah-hah")
  }
  
  it should "remove leading trailing spaces" in { 
    processStyleName(" \nblahHah \n\t") should equal ("blah-hah")
  }
  
  it should "process the special mozilla prefix special" in { 
    processStyleName(" \n msBlahHah") should equal ("-ms-blah-hah")
  }
  
  "quoteValueForStyle" should "return empty string for a boolean" in { 
    quoteStyleValueForBrowser(None, true) should equal ("")
  }
  
  it should "return empty string for a null value" in { 
    quoteStyleValueForBrowser(None, null) should equal ("")
  }
  
  it should "return a unitless string if the hint says its unitless" in { 
    quoteStyleValueForBrowser(Some(StyleHint(values = BitSet(Hints.Unitless))), "200") should equal ("200")
  }
  
  it should "return a string with units if the hint says its not unitless" in { 
    quoteStyleValueForBrowser(None, "200") should equal ("200px")
  }
  
  "adler32" should "calculate a checksum" in { 
    val checksum = adler32("<div>blah is cool</div>")
    //println(s"$checksum")
    checksum should be (1628178442)
  }
  
  it should "add it to markup correctly" in { 
    val markup= addChecksumToMarkup("<div>blah is cool</div>")
    markup should equal ("""<div data-scala-vdom-checksum="1628178442">blah is cool</div>""")
  }
 
}
