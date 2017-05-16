package ru.simplesys.plugins.sourcegen.app.Gen

import java.net.URI

import com.simplesys.common.Strings.{newLine, _}
import com.simplesys.common._
import com.simplesys.genSources._
import com.simplesys.io._
import com.simplesys.scalaGen._
import com.simplesys.xhtml.XHTML._
import ru.simplesys.plugins.sourcegen.app.xml.IscElem
import sbt.{File, Logger}

import scala.collection.mutable.ArrayBuffer
import scalax.file.{Path, PathSet}

class GenScalaApp(val appFilePath: Path,
                  val schemaPath: URI,
                  val outFilePath: Path,
                  val packageName: String,
                  val stage: String,
                  val logger: Logger) extends GenScala1 {

    val generetedFiles: PathSet[Path] = appFilePath * "*.xml"

    private def makeCollectioISCElements(parentElem: IscElem, collectionElem: ArrayBuffer[ScalaElement], modificator: ModificatorVariable) {
        for (element <- parentElem.child.filter(_.label != "#PCDATA")) {
            val _element: IscElem = element
            val propertyElement: ScalaClassJSONProperty = _element.getScalaClassJSONProperty

            propertyElement match {
                case ScalaClassJSONProperty(label, scalaPropertyElement) =>
                    scalaPropertyElement match {
                        case ScalaClassJSONPropertyClassJSON(value) =>
                            value addProperties makeCode(_element)
                            val id: IscElem = _element \ "Identifier"
                            //id.log
                            if (id.value.isEmpty)
                                throw new RuntimeException(s"Unknown Identifier for type : ${scalaPropertyElement.toString.dblQuoted}")
                            modificator match {
                                case PrivateMofificator =>
                                    collectionElem += (id.value, ScalaBody(("" -> value).property)).lazyPrivateVal

                                case ProtectedMofificator =>
                                    collectionElem += (id.value, ScalaBody(("" -> value).property)).lazyProtectedVal
                            }

                        case _ =>
                            throw new RuntimeException(s"Unknown implemantation for type : ${scalaPropertyElement.toString.dblQuoted}")
                    }
                case _ =>
                    throw new RuntimeException(s"Unknown implemantation for type : ${propertyElement.toString.dblQuoted}")
            }
        }
    }

    private def makeCollectionISCElements(parentElem: IscElem, collectionElem: ArrayBuffer[(String, ScalaElement)]): String = {
        var res = ""
        for (element <- parentElem.child.filter(_.label != "#PCDATA")) {
            val _element: IscElem = element
            val propertyElement: ScalaClassJSONProperty = _element.getScalaClassJSONProperty

            propertyElement match {
                case ScalaClassJSONProperty(label, scalaPropertyElement) =>
                    scalaPropertyElement match {
                        case ScalaClassJSONPropertyClassJSON(value) =>
                            value addProperties makeCode(_element)
                            val id: IscElem = _element \ "Identifier"
                            res += (_element \ "ID").text.dblQuoted + space + ","
                            //id.log

                            (collectionElem exists { case (name, _) => name == id.value }) match {
                                case false =>
                                    collectionElem += Tuple2(id.value, (id.value, ScalaBody(("" -> value).property)).lazyPrivateVal)
                                    collectionElem += Tuple2(id.value + "newLine1", newLine)
                                case _ =>
                            }

                        case _ =>
                            throw new RuntimeException(s"Unknown implemantation for type : ${scalaPropertyElement.toString.dblQuoted}")
                    }
                case _ =>
                    throw new RuntimeException(s"Unknown implemantation for type : ${propertyElement.toString.dblQuoted}")
            }
        }
        res.delLastChar
    }

    private def makeCollectionISCElementsJS(parentElem: IscElem, collectionElem: ArrayBuffer[(String, ScalaElement)]): String = {
        var res = ""
        for (element <- parentElem.child.filter(_.label != "#PCDATA")) {
            val _element: IscElem = element
            val propertyElement: ScalaClassJSONProperty = _element.getScalaClassJSONPropertyJS

            propertyElement match {
                case ScalaClassJSONProperty(label, scalaPropertyElement) =>
                    scalaPropertyElement match {
                        case ScalaClassJSONPropertyClassJSON(value) =>
                            value.wrappadOperator = "RestDataSourceSS.create"
                            value addProperties makeCodeJS(_element)
                            val id: IscElem = _element \ "Identifier"
                            res += (_element \ "ID").text.dblQuoted + space + ","
                            //id.log

                            (collectionElem exists { case (name, _) => name == id.value }) match {
                                case false =>
                                    collectionElem += Tuple2(id.value, (id.value, ScalaBody(("" -> value).property)).publicVal)
                                    collectionElem += Tuple2(id.value + "newLine1", newLine)
                                case _ =>
                            }

                        case _ =>
                            throw new RuntimeException(s"Unknown implemantation for type : ${scalaPropertyElement.toString.dblQuoted}")
                    }
                case _ =>
                    throw new RuntimeException(s"Unknown implemantation for type : ${propertyElement.toString.dblQuoted}")
            }
        }
        res.delLastChar
    }

    def create: File = ????

    def createSeq: Seq[File] = {
        val resSeq = ArrayBuffer.empty[File]

        val dataSourcesJS = new ScalaClassDeclare {
            scalaClassGen = "DataSourcesJS".cls
            annotation = ScalaAnnotation("JSExport")
            typeScalaClass = TypeScalaObject
        }

        val dssJS = ArrayBuffer.empty[(String, ScalaElement)]

        generetedFiles foreach {
            file =>

                val mns = ArrayBuffer.empty[ScalaElement]
                val cnvs = ArrayBuffer.empty[ScalaElement]

                val root: IscElem = loadFile(file.toFile, schemaPath)

                val boName = file.onlyFileName

                for (xmlPiece <- root.child) {
                    val pane: IscElem = xmlPiece
                    //pane.log

                    pane.label match {
                        case "DataSources" =>
                            makeCollectionISCElementsJS(pane, dssJS)

                        case "Menus" =>
                            makeCollectioISCElements(pane, mns, ProtectedMofificator)
                            mns += newLine
                            mns += newLine

                        case "BoName" | "GroupName" | "JObjectName" | "Bo" | "MenuPath" =>

                        case "RootCanvas" =>
                            makeCollectioISCElements(pane, cnvs, ProtectedMofificator)

                        case label =>
                            throw new RuntimeException(s"Unknown implemantation for pane.label : ${label.dblQuoted}")

                    }
                }
        }

        dataSourcesJS addMember ScalaMethod(
            name = "anyToScOpt1",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaString)),
            //`type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        dataSourcesJS addMember ScalaMethod(
            name = "anyToScOpt2",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaDouble)),
            //`type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        dataSourcesJS addMember ScalaMethod(
            name = "anyToScOpt3",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaBoolean)),
            //`type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        dataSourcesJS addMember ScalaMethod(
            name = "anyToScOpt4",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaInt)),
            //`type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        dataSourcesJS addMember ScalaMethod(
            name = "anyToScOpt5",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaString)),
            `type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        dataSourcesJS addMember ScalaMethod(
            name = "anyToScOpt6",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaDouble)),
            `type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        dataSourcesJS addMember ScalaMethod(
            name = "anyToScOpt7",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaBoolean)),
            `type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        dataSourcesJS addMember ScalaMethod(
            name = "anyToScOpt8",
            serrializeToOneString = true,
            `final` = ImplicitMofificator,
            parametrs = ScalaClassParametrs(ScalaClassParametr(name = "x", `type` = ScalaInt)),
            `type` = "ScOption[JSAny]".tp,
            body = ScalaBody("ScSome(x)"))

        dataSourcesJS addMember newLine

        dataSourcesJS ++= dssJS.map { case (_, scalaElement) => scalaElement }

        val moduleDataSourcesJS = new ScalaModule(
            (packageName + ".ScalaJSGen").pkg,
            newLine,
            "com.simplesys.SmartClient.DataBinding.props.RestDataSourceSSProps".imp,
            "com.simplesys.SmartClient.DataBinding.props.dataSource.{WildRecordColumnProps, DataSourceFieldProps, OperationBindingProps}".imp,
            "com.simplesys.SmartClient.System.RestDataSourceSS".imp,
            "com.simplesys.option.{ScOption, ScSome}".imp,
            "com.simplesys.option.ScOption._".imp,
            "com.simplesys.System.JSAny".imp,
            "com.simplesys.System.Types._".imp,
            newLine,
            "scala.scalajs.js.annotation.JSExport".imp,
            newLine,
            dataSourcesJS
        )

        val outFileDataSourcesJS: Path = outFilePath / "DataSourcesJS.scala"
        val resDataSourcesJS = outFileDataSourcesJS.createFile(failIfExists = false).toFile

        resDataSourcesJS <== {
            out =>
                out(genMessageCreating(s"GenScalaApp (createSeq), stage: $stage"))
                out(newLine)
                out(moduleDataSourcesJS.serrialize())
        }

        resSeq ++= Seq(resDataSourcesJS)
    }
}
