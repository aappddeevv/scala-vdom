package org.im
package vdom

import utest._

object PatchTestsSuite extends TestSuite {

  val tests = TestSuite {
    "test1"{
      throw new Exception("test1")
    }
    "test2"{
      1
    }
    "test3"{
      val a = List[Byte](1, 2)
      a(10)
    }
  }
}
