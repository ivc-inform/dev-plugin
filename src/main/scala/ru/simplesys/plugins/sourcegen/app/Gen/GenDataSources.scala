package ru.simplesys.plugins.sourcegen.app.Gen

import java.net.URI

import com.simplesys.common.Strings.{newLine, _}
import com.simplesys.common._
import com.simplesys.genSources._
import com.simplesys.io._
import com.simplesys.scalaGen.{ScalaClassesJSON, _}
import com.simplesys.xhtml.XHTML._
import ru.simplesys.plugins.sourcegen.app.xml.IscElem
import sbt.{File, Logger}

import scala.collection.mutable.ArrayBuffer
import com.simplesys.file.{Path, PathSet}
import ru.simplesys.plugins.sourcegen.app.SeqScalaClassJSON

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
                            val properties: ScalaClassJSONProperties = ScalaClassJSONProperties(makeCodeJS(_element).getItems.map {
                                case ScalaClassJSONProperty(key, value) if key == "fields" ⇒
                                    value match {
                                        case cls: SeqScalaClassJSON ⇒
                                            val a = new SeqScalaClassJSON(cls.getOpt, cls.getItems.map {
                                                case item: ScalaClassJSON ⇒
                                                    var name: Option[String] = None
                                                    var lookup: Option[Boolean] = None
                                                    var foreignField: Option[String] = None

                                                    item.properties.getItems.foreach {
                                                        case ScalaClassJSONProperty(key, value) ⇒
                                                            value match {
                                                                case ScalaClassJSONPropertyString(value) if key == "name" ⇒
                                                                    name = Some(value)
                                                                case ScalaClassJSONPropertyString(value) if key == "foreignField" ⇒
                                                                    foreignField = Some(value)
                                                                case ScalaClassJSONPropertyBoolean(value) if key == "lookup" ⇒
                                                                    lookup = Some(value)
                                                                case _ ⇒
                                                            }
                                                    }

                                                    item.properties = ScalaClassJSONProperties(item.properties.getItems.map {
                                                        case ScalaClassJSONProperty(key, value) ⇒
                                                            value match {
                                                                case ScalaClassJSONPropertyString(value) if key == "name" ⇒
                                                                    if (name.isDefined && lookup.isDefined && foreignField.isDefined)
                                                                        ScalaClassJSONProperty(key, ScalaClassJSONPropertyString(s"${name.get}_${foreignField.get.capitalize}"))
                                                                    else
                                                                        ScalaClassJSONProperty(key, ScalaClassJSONPropertyString(value))
                                                                case _ ⇒
                                                                    ScalaClassJSONProperty(key, value)
                                                            }
                                                    }: _*)

                                                    item
                                            }: _*)
                                            ScalaClassJSONProperty(key, a)
                                        case _ ⇒
                                            ScalaClassJSONProperty(key, value)
                                    }
                                case x ⇒ x
                            }: _*)
                            value addProperties properties
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
                out(genMessageCreating(s"GenDataSources (createSeq), stage: $stage"))
                out(newLine)
                out(moduleDataSourcesJS.serrialize())
        }

        resSeq ++= Seq(resDataSourcesJS)
    }
}
