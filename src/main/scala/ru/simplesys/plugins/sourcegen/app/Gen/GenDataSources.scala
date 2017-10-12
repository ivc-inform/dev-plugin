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
import com.simplesys.file.{Path, PathSet}

class GenDataSources(val appFilePath: Path,
                     val schemaPath: URI,
                     val outFilePath: Path,
                     val packageName: String,
                     val stage: String,
                     val logger: Logger) extends GenScala1 {

    val generetedFiles: PathSet[Path] = appFilePath * "dataSources.xml"

    private def makeCollectionISCElementsJS(parentElem: IscElem, collectionElem: ArrayBuffer[(String, ScalaElement)]) = {
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
    }

    def create: File = ????

    def createSeq: Seq[File] = {
        val resSeq = ArrayBuffer.empty[File]

        val dataSourcesJS = new ScalaClassDeclare {
            scalaClassGen = "DataSourcesJS".cls
            //annotation = ScalaAnnotation("JSExport")
            typeScalaClass = TypeScalaObject
            extensibleClass = "Implicits".ext
        }

        val dssJS = ArrayBuffer.empty[(String, ScalaElement)]

        generetedFiles foreach {
            file =>
                val root: IscElem = loadFile(file.toFile, schemaPath)

                root.label match {
                    case "DataSources" =>
                        makeCollectionISCElementsJS(root, dssJS)

                    case label =>
                        throw new RuntimeException(s"Unknown implemantation for root.label : ${label.dblQuoted}")
                }
        }

        dataSourcesJS ++= dssJS.map { case (_, scalaElement) => scalaElement }

        val moduleDataSourcesJS = new ScalaModule(
            (packageName + ".ScalaJSGen").pkg,
            newLine,
            "com.simplesys.SmartClient.DataBinding.props.RestDataSourceSSProps".imp,
            "com.simplesys.SmartClient.DataBinding.props.dataSource.{WildRecordColumnProps, DataSourceFieldProps, OperationBindingProps}".imp,
            "com.simplesys.SmartClient.System.{Implicits, RestDataSourceSS}".imp,
            "com.simplesys.option.{ScOption, ScSome}".imp,
            "com.simplesys.option.ScOption._".imp,
            "com.simplesys.System.JSAny".imp,
            "com.simplesys.System.Types._".imp,
            newLine,
            dataSourcesJS
        )

        val outFileDataSourcesJS: Path = outFilePath / "DataSourcesJS.scala"
        val resDataSourcesJS = outFileDataSourcesJS.createFile(failIfExists = false).toFile

        resDataSourcesJS <== {
            out =>
                out(genMessageCreating(s"GenDataSources, stage: $stage"))
                out(newLine)
                out(moduleDataSourcesJS.serrialize())
        }

        resSeq ++= Seq(resDataSourcesJS)
    }
}
