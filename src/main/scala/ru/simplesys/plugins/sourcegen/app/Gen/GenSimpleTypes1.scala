package ru.simplesys.plugins.sourcegen.app.Gen

import java.net.URI

import com.simplesys.common.Strings.{newLine, _}
import com.simplesys.genSources._
import com.simplesys.io._
import com.simplesys.scalaGen._
import com.simplesys.xhtml.XHTML._
import ru.simplesys.plugins.sourcegen.app.xml.IscElem
import sbt.{File, Logger}

import scalax.file.Path

class GenSimpleTypes1(val appFilePath: Path,
                      val schemaPath: URI,
                      val outFilePath: Path,
                      val packageName: String,
                      val stage: String,
                      val logger: Logger) extends GenScala {

    override def create: File = {
        val root: IscElem = loadFile(appFilePath.toFile, schemaPath)

        val simpleTypes = new ScalaClassDeclare {
            scalaClassGen = "SimpleTypes".cls
            //typeScalaClass = TypeScalaObject
            extensibleClass = "StaticJSCode".ext
            annotation = ScalaAnnotation("JSExport")
        }

        simpleTypes addMember ScalaMethod(
            name = "anyToScOpt1",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaString)),
            //`type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        simpleTypes addMember ScalaMethod(
            name = "anyToScOpt2",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaDouble)),
            //`type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        simpleTypes addMember ScalaMethod(
            name = "anyToScOpt3",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaBoolean)),
            //`type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        simpleTypes addMember ScalaMethod(
            name = "anyToScOpt4",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaInt)),
            //`type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        simpleTypes addMember ScalaMethod(
            name = "anyToScOpt5",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaString)),
            `type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        simpleTypes addMember ScalaMethod(
            name = "anyToScOpt6",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaDouble)),
            `type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        simpleTypes addMember ScalaMethod(
            name = "anyToScOpt7",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaBoolean)),
            `type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        simpleTypes addMember ScalaMethod(
            name = "anyToScOpt8",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaInt)),
            `type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        simpleTypes addMember newLine

        //val bodyCreateJS = ScalaBody(s"isc debugTrap ${genMessageCreating(s"GenSimpleTypes1, stage: $stage").dblQuoted}")
        val bodyCreateJS = ScalaBody()

        for (xmlPiece <- root.child) {
            val simpleType: IscElem = xmlPiece
            simpleType.label match {
                case "SimpleTypeDyn" =>
                    val elem: String = ((simpleType \ "Name"): IscElem).value
                    bodyCreateJS += ScalaExpression(expression = makeScalaCodeJS(simpleType).serrialize())
                    bodyCreateJS += newLine
                case label =>
                    throw new RuntimeException(s"Unknown implemantation for simpleType.label : ${label.dblQuoted}")
            }
        }

        simpleTypes addMember ScalaMethod(
            name = "createJS",
            body = bodyCreateJS,
            `type` = ScalaUnit,
            parametrs = ScalaClassParametrs(emptyparentheses = true),
            annotation = ScalaAnnotation("JSExport")
        )

        simpleTypes addMember newLine

        val module = new ScalaModule(
            (packageName + ".jsGen").pkg,
            "com.simplesys.SmartClient.DataBinding.props.SimpleTypeProps".imp,
            "com.simplesys.SmartClient.System.{SimpleType, isc}".imp,
            "com.simplesys.System.{JSAny, JSUndefined, jSUndefined}".imp,
            "com.simplesys.System.Types.OperatorId".imp,
            "com.simplesys.option.{ScOption, ScSome}".imp,
            "com.simplesys.option.ScOption._".imp,
            "com.simplesys.SmartClient.App.StaticJSCode".imp,
            "scala.scalajs.js.annotation.JSExport".imp,
            newLine,
            simpleTypes
        )

        val res = outFilePath.createFile(failIfExists = false).toFile

        res <== {
            out =>
                out(genMessageCreating(s"GenSimpleTypes1, stage: $stage"))
                out(newLine)
                out(org.scalafmt.Scalafmt.format(module.serrialize()).get)
        }
    }
}
