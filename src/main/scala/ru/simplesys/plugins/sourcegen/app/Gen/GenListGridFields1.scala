package ru.simplesys.plugins.sourcegen.app.Gen

import java.net.URI

import com.simplesys.common.Strings.{newLine, _}
import com.simplesys.common._
import com.simplesys.file.{Path, PathSet}
import com.simplesys.genSources._
import com.simplesys.io._
import com.simplesys.scalaGen._
import com.simplesys.xhtml.XHTML._
import ru.simplesys.plugins.sourcegen.app.xml.IscElem
import sbt.{File, Logger}

import scala.collection.mutable.ArrayBuffer

class GenListGridFields1(val appFilePath: Path,
                         val schemaPath: URI,
                         val outFilePath: Path,
                         val packageName: String,
                         val stage: String,
                         val logger: Logger) extends GenScala1 {

    val generetedFiles: PathSet[Path] = appFilePath * "dataSources.xml"
    type CollectionElem = ArrayBuffer[(String, ScalaElement)]
    type CollectionElemObject = ArrayBuffer[ScalaObjectElement]
    type CollectionElemName = ArrayBuffer[String]

    private def makeCollectionISCElementsJS(parentElem: IscElem, listGridFieldsCollection: CollectionElem, formItemsCollection: CollectionElem, collectionElemObject: CollectionElemObject, collectionElemName: CollectionElemName) = {
        for (element <- parentElem.child.filter(_.label != "#PCDATA")) {
            val fields: IscElem = element \ "Fields"

            val id: IscElem = element \ "Identifier"
            val nameBase = id.value.replace("DS", "")
            (listGridFieldsCollection exists { case (name, _) => name == id.value }) match {
                case false =>
                    for (elementField <- fields.child) {
                        val _elementField: IscElem = elementField
                        val fieldName = (elementField \ "Name").text
                        val lookup = _elementField.getBooleanValue("Lookup")
                        val foreignKey = _elementField.getStringValue("ForeignField")

                        val listFridFieldObjectName = if (!lookup) s"${nameBase}${fieldName}_NameStrong" else s"${nameBase}${fieldName}_${foreignKey.capitalize}_NameStrong"

                        if (collectionElemName.find(_ == listFridFieldObjectName).isEmpty) {
                            collectionElemName += listFridFieldObjectName
                            val listFridFieldObject = new ScalaClassDeclare {
                                scalaClassGen = listFridFieldObjectName.cls
                                typeScalaClass = TypeScalaObject
                            }
                            if (!lookup)
                                listFridFieldObject addMember (ScalaVariable(name = "name", body = s"${
                                    {
                                        import com.simplesys.common.JVM.Strings._
                                        fieldName.dblQuoted
                                    }}".body, serrializeToOneString = true))
                            else
                                listFridFieldObject addMember (ScalaVariable(name = "name", body = {
                                    import com.simplesys.common.JVM.Strings._
                                    s"${fieldName}_${foreignKey.capitalize}".dblQuoted.body
                                }, serrializeToOneString = true))
                            collectionElemObject += listFridFieldObject
                        }
                    }
                case _ =>
            }
        }
    }

    def create: File = ????

    def createSeq: Seq[File] = {
        val resSeq = ArrayBuffer.empty[File]

        val lss = ArrayBuffer.empty[(String, ScalaElement)]
        val lsf = ArrayBuffer.empty[(String, ScalaElement)]
        val lssObjects = ArrayBuffer.empty[ScalaObjectElement]
        val lssNames = ArrayBuffer.empty[String]

        generetedFiles foreach {
            file =>
                val root: IscElem = loadFile(file.toFile, schemaPath)

                root.label match {
                    case "DataSources" =>
                        makeCollectionISCElementsJS(root, lss, lsf, lssObjects, lssNames)

                    case label =>
                        throw new RuntimeException(s"Unknown implemantation for root.label : ${
                            {
                                import com.simplesys.common.JVM.Strings._
                                label.dblQuoted
                            }}")
                }
        }

        val moduleDataSourcesJS = new ScalaModule(
            (packageName + ".ScalaJSGen").pkg,
            newLine
        )

        moduleDataSourcesJS ++= lssObjects.toArray

        val outFileDataSourcesJS: Path = outFilePath / "ListGridFiledsJS.scala"
        val resDataSourcesJS = outFileDataSourcesJS.createFile(failIfExists = false).toFile

        resDataSourcesJS <== {
            out =>
                out(genMessageCreating(s"GenListGridFields (createSeq), stage: $stage"))
                out(newLine)
                out(org.scalafmt.Scalafmt.format(moduleDataSourcesJS.serrialize()).get)
        }

        resSeq += resDataSourcesJS
    }
}
