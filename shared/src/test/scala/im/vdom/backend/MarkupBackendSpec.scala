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
 * A test backend is used to test IOAction run funcitonality.
 */
class MarkupBackendSpec extends AsyncFlatSpec with Matchers with OptionValues {

  import Utils._
  import VNode._

  val b = MarkupBackend
  import HTML5Attributes._

  "MarkupBackend" should "run a successful action" in {
    val action = Action.successful(true)
    val result = b.run(action)
    result.map { result =>
      result should be(true)
    }
  }

  it should "render a simple tag" in {
    val v = tag("div")
    b.run(b.render(v)).map { html =>
      norm(html) should be("<DIV></DIV>")
    }
  }

  it should "render a tag with insane whitespace around it" in {
    val v = tag("  div  \t\n ")
    b.run(b.render(v)).map { html =>
      norm(html) should be("<DIV></DIV>")
    }
  }

  it should "render a tag with a string attribute" in {
    val v = tag("div", Seq(id := "foo"))
    b.run(b.render(v)).map { html =>
      norm(html) should be("""<DIV ID="FOO"></DIV>""")
    }
  }

  it should "render a tag with a string attribute and a text node" in {
    val v = tag("div", Seq(id := "foo"), text("FOO"))
    b.run(b.render(v)).map { html =>
      norm(html) should equal("""<DIV ID="FOO">FOO</DIV>""")
    }
  }

  it should "render a tage with child elements" in {
    val v = tag("div", tag("p", text("foo")), tag("div", text("bar")))
    b.run(b.render(v)).map { html =>
      norm(html) should equal("""<DIV><P>FOO</P><DIV>BAR</DIV></DIV>""")
    }
  }

  it should "omit a closing tag for br" in {
    val v = tag("br")
    b.run(b.render(v)).map { html =>
      norm(html) should equal("""<BR/>""")
    }
  }

  it should "not render an HasBooleanValue=true attribute when the attribute is false" in {
    val v = tag("div", Seq(disabled := false))
    b.run(b.render(v)).map { html =>
      norm(html) should equal("""<DIV></DIV>""")
    }
  }

  // Not sure this should render to ="" or to just the name of the attribute per HTML5 spec
  it should "render a HasBooleanValue=true attribute when the attribute is true" in {
    val v = tag("div", Seq(disabled := true))
    b.run(b.render(v)).map { html =>
      norm(html) should equal("""<DIV DISABLED=""></DIV>""")
    }
  }
  
  it should "render a simple style" in { 
    val v = tag("div", Seq(Styles.height := 200))
    b.run(b.render(v)).map { html =>
      norm(html) should equal ("""<DIV STYLE="HEIGHT:200PX;"></DIV>""")
    }
  }
  
  it should "render multple styles" in { 
    val v = tag("div", Seq(Styles.height := 200, Styles.width := 400, Styles.zIndex := 3))
    b.run(b.render(v)).map { html =>
      norm(html) should equal ("""<DIV STYLE="HEIGHT:200PX;WIDTH:400PX;Z-INDEX:3;"></DIV>""")
    }
  }
   
}
