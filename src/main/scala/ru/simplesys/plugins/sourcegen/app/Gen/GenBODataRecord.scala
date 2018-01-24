package ru.simplesys.plugins.sourcegen.app.Gen

import java.net.URI

import com.simplesys.common.Strings._
import com.simplesys.common._
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.file.{Path, PathSet}
import com.simplesys.io._
import com.simplesys.scalaGen._
import com.simplesys.xhtml.XHTML._
import ru.simplesys.plugins.sourcegen.app.xml.IscElem
import ru.simplesys.plugins.sourcegen.meta._
import sbt.{File, Logger}
import com.simplesys.genSources._

import scala.collection.mutable.ArrayBuffer

class GenBODataRecord(val appFilePath: Path,
                      val boFilePath: Path,
                      val schemaPath: URI,
                      val sourceMain: Path,
                      val outFilePath: Path,
                      val packageName: String,
                      val pkgBOName: String,
                      val stage: String,
                      val logger: Logger) extends GenScala1 {

    val operationTypes = Seq("Add", "Fetch", "Remove", "Update")
    val sourceBOFiles: PathSet[Path] = boFilePath * "*.xml"
    val generetedFiles: PathSet[Path] = appFilePath * "dataSources.xml"

    def create: File = ????

    def createSeq: Seq[File] = {
        implicit val schema = SchemaDef(pkgBOName, sourceBOFiles.files)

        val resSeq = ArrayBuffer.empty[File]
        val servletes = ArrayBuffer.empty[String]
        val res = (outFilePath / "scala" / "container" / "RecordsBOs.scala").createFile(failIfExists = false).toFile

        generetedFiles foreach {
            file =>
                val traitsRecords = ArrayBuffer.empty[ScalaObjectElement]

                val root: IscElem = loadFile(file.toFile, schemaPath)
                val dataSources: IscElem = root

                for (_dataSource <- dataSources.child) {

                    val dataSource: IscElem = _dataSource
                    val dataSourceIdentifier = (dataSource \ "Identifier").text.replace("_DS", "")

                    if (dataSourceIdentifier.isEmpty)
                        throw new RuntimeException("Не определен Identifier для DataSource.")

                    if (!servletes.exists(_ === dataSourceIdentifier)) {
                        servletes += dataSourceIdentifier
                        val boName = dataSourceIdentifier.substring(dataSourceIdentifier.indexOf("_") + 1)

                        val recordTrait = new ScalaClassDeclare {
                            scalaClassGen = (s"${boName.capitalize}DataRecord").cls
                            typeScalaClass = TypeScalaTrait
                            extensibleClass = "js.Object".ext
                        }

                        (_dataSource \ "Fields" \ "DataSourceFieldDyn") foreach {
                            x =>
                                val name = (x: IscElem).getStringValue("Name")
                                val tp: String = (x: IscElem).getStringValue("GetterType").replace("Opt", strEmpty)
                                val required: Boolean = (x: IscElem).getBooleanValue("Required")
                                val lookup: Boolean = (x: IscElem).getBooleanValue("Lookup")

                                if (!lookup) {
                                    val _tp = tp match {
                                        case "Long" ⇒ "Double"
                                        case any ⇒ any
                                    }
                                    recordTrait addMember ScalaVariable(name = name, serrializeToOneString = true, sign = strEmpty, `type` = s"js.UndefOr[${_tp}]".tp, body = "= js.undefined".body)
                                }
                        }

                        traitsRecords append recordTrait
                        traitsRecords append newLine

                    }
                }

                val module = ScalaModule(
                    s"ru.simplesys.defs.app.scala.container".pkg,
                    newLine,
                    "java.time.LocalDateTime".imp,
                    "scala.scalajs.js".imp,
                    "com.simplesys.jdbc.control.clob.Blob".imp
                )


                module ++= (traitsRecords: _*)

                res <== {
                    out =>
                        out(genMessageCreating(s"GenBOContainer, stage: $stage"))
                        out(newLine)
                        out(newLine)
                        out(org.scalafmt.Scalafmt.format(module.serrialize()).get)
                }

                resSeq += res
        }

        resSeq
    }

}
