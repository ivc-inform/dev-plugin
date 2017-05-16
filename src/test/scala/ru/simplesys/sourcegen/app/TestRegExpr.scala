package ru.simplesys.sourcegen.app

import org.scalatest.FunSuite
import com.simplesys.log.Logging
import com.simplesys.scalaGen._

class TestRegExpr extends FunSuite with Logging {
  test("1") {
    val reg = """(-)?(\d+)(\.\d*)?""".r

    val input = "for -1.0 to 99 by 3"
    for (reg(s, i, d) <- reg findAllIn input)
      println(s)
  }

  /*test("test select Function from JS File") {
    val str = getAllJSCode("MenuItemsFunctions.js", 4)
    val reg = """(\n*)(\s*function\s+)(\w+\s*)(\((\s*\w+\s*,?)*\s*\))(\s*\{[\n\s\w\d=\.;,/|\(\)]*\})""".r

    for (reg(a, function, functionName, params, _, body) <- reg findAllIn str/* if functionName == "saveRecords"*/) {
      //for (a <- reg findAllIn str){
      println(function + params + body)
    }
  }*/
}