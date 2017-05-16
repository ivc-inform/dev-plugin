package ru.simplesys.sourcegen.app

import com.simplesys.common.Strings._
import com.simplesys.common._
import com.simplesys.log.Logging
import com.simplesys.scalaGen._
import com.simplesys.xhtml.XHTML
import com.simplesys.xml.Elem
import org.scalatest.FunSuite
import ru.simplesys.plugins.sourcegen.app._
import ru.simplesys.plugins.sourcegen.app.xml.IscElem
import sbt.{Level, Logger}

//import org.jooq.util.GenerationTool

class Tests extends FunSuite with Logging {

    val _indent = 0

    val parametrs = ScalaClassParametrs(("a", ScalaString).param, ("b", ScalaInt).param)
    val method = ScalaMethod(
        name = "Method1",
        parametrs = parametrs,
        body = ScalaBody( """val res = (parametrs map (item => item.serrialize)).mkString(",")
                  if (!res.isEmpty)
                    "(" + res + ")"
                  else """""))

    val method2 = ScalaMethod(
        name = "Method2",
        parametrs = parametrs,
        `type` = ScalaString,
        body = ScalaBody( """val res = (parametrs map (item => item.serrialize)).mkString(",")
                  if (!res.isEmpty)
                    "(" + res + ")"
                  else
                          """""))


    val valVal = ("variable1", """JsonObject() + ("a" -> 1) + ("b" -> 3)""".body).`val`
    val valVar = ("variable2", ScalaInt, """JsonObject() + ("a" -> 1) + ("b" -> 3)""".body).`var`

    implicit val _logger = new Logger {
        def log(level: Level.Value, message: => String) {
            logger debug message
        }
        def success(message: => String) {
            logger debug message
        }
        def trace(t: => Throwable) {
            logger debug t
        }
    }

    test("ScalaClassJSONPropertyJSCode") {
        val a = ScalaClassJSONPropertyJSCode("MenuItemsFunctions.js")
        logger debug (newLine + a.serrialize())
    }

    test("Create DataSource") {
        val a = new ScalaClassJSON {
            scalaClassGen = "DataSourceFieldDyn".cls
            properties = ScalaClassJSONProperties(
                ("Title" -> "Заголовок".dblQuotedValue).property,
                ("PrimaryKey" -> true).property,
                ("Type" -> "ftFloat".str).property
            )
        }

        val b = new ScalaClassJSON {
            scalaClassGen = "DataSourceFieldDyn".cls
            properties = ScalaClassJSONProperties(
                ("Title" -> "Заголовок 2").property,
                ("PrimaryKey" -> true).property,
                ("Length" -> 255).property,
                ("Length1" -> 255.5).property,
                ("Type" -> "ftFloat".str).property
            )
        }

        val restDataSource = new ScalaClassJSON {
            scalaClassGen = "RestDataSourceDyn".cls
            parametrs = ScalaClassInitParametrs(("useSelfName", "true").param)
            properties = ScalaClassJSONProperties(
                ("DataFormat" -> "dtftJSON").property,
                ("JsonPrefix" -> "").property,
                ("Fields" -> ArrayScalaClassJSON(a, b)).property
            )
        }
    }

    test("If") {
        val s = Seq("a = 0", "b = a")
        logger debug ScalaIf("a = b".expr, s.body, "b = 0".body, serrializeToOneString = false).serrialize(4)
        logger debug ScalaIf("a = b".expr, s.body, serrializeToOneString = true).serrialize(4)
    }

    test("Case") {
        val s = Seq("a = 0", "b = a")
        //val a = ScalaCaseLine(expression = "'+'".expr, caseBody = "1".body)
        val a = ScalaCaseLine(expression = "'+'".expr, guard = ScalaIf("a = b".expr, thenBody = s.body, serrializeToOneString = true), caseBody = s.body)

        logger debug a.serrialize()
        val c = ScalaCase("ch".expr,
            ScalaCaseLine(expression = "'+'".expr, guard = ScalaIf("a = b".expr, serrializeToOneString = true), caseBody = s.body),
            ScalaCaseLine(expression = "'+'".expr, guard = ScalaIf("a = b".expr, serrializeToOneString = true), caseBody = s.body))
        logger debug c.serrialize(4)
    }


    /*val function = RequestResponse {
                    request =>
                        FutureResponse {
                            response =>
                                getPage(request = request, response = response, contextPath = ServletContext.ContextPath)
                        }
                }*/

    test("ScalaControlStruct") {
        val cs = ScalaVariable(name = "function", body = ScalaBody(
            ScalaControlStruct(name = "RequestResponse", body = ScalaControlBody(expression = "request".expr,
                ScalaControlStruct(name = "FutureResponse", body = ScalaControlBody(expression = "response".expr,
                    "getPage(request = request, response = response, contextPath = ServletContext.ContextPath)"))))))

        logger debug (newLine + cs.serrialize(0))
    }

    test("Generate Schema Scala Code") {
        var uri = URIBox getResource "defs/app/generated/user.xml"
        var uriSchema = URIBox getResource "defs/app/xsd/schemaApp.xsd"

        val s: Elem = XHTML loadFromUR (uriSchema)
        val e: Elem = XHTML loadFromURSeq(uri, uriSchema)

        //val e: Elem = XHTML loadFromURI(uri)
        ((e \ "Native"): IscElem).log
    }


    test("userTrd.xml") {
        var uri = URIBox getResource "defs/Trd/userTrd.xml"
        var uriSchema = URIBox getResource "defs/app/ds/schemaDS.xsd"

        val e: Elem = XHTML loadFromUR(uri, uriSchema)
        //val s: Elem = XHTML loadFromUR (uriSchema)

        ((e \ "Native"): IscElem).log
    }

    /*test("Test transform 3") {

        val boPath: Path = "/home/andrew/JOB/enzo/src/main/resources/defs/bo"
        val appPath: Path = "/home/andrew/JOB/enzo/src/main/resources/defs/app"
        val outPath: Path = "/home/andrew/JOB/enzo/target/scala-2.10/src_managed/main/defs/app"

        new _GenMegedAllBoModel(
            appFilePath = appPath,
            boFilePath = boPath,
            outFilePath = outPath,
            logger = _logger).create
    }*/

}