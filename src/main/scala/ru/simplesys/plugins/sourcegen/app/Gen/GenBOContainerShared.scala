package ru.simplesys.plugins.sourcegen.app.Gen

import java.net.URI

import com.simplesys.common.Strings._
import com.simplesys.common._
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.file.{Path, PathSet}
import com.simplesys.genSources._
import com.simplesys.io._
import com.simplesys.scalaGen._
import com.simplesys.xhtml.XHTML._
import ru.simplesys.plugins.sourcegen.app.xml.IscElem
import ru.simplesys.plugins.sourcegen.meta._
import sbt.{File, Logger}

import scala.collection.mutable.ArrayBuffer

class GenBOContainerShared(val appFilePath: Path,
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

        generetedFiles foreach {
            file =>
                val root: IscElem = loadFile(file.toFile, schemaPath)
                val dataSources: IscElem = root

                for (_dataSource <- dataSources.child) {
                    val addedImports = ScalaImports()

                    val dataSource: IscElem = _dataSource
                    val dataSourceIdentifier = (dataSource \ "Identifier").text.replace("_DS", "")

                    if (dataSourceIdentifier.isEmpty)
                        throw new RuntimeException("Не определен Identifier для DataSource.")

                    if (!servletes.exists(_ === dataSourceIdentifier)) {
                        servletes += dataSourceIdentifier
                        val boName = dataSourceIdentifier.substring(dataSourceIdentifier.indexOf("_") + 1)
                        val groupName = dataSourceIdentifier.substring(0, dataSourceIdentifier.indexOf("_"))


                        val firstAddImport = s"${pkgBOName}.${groupName}._".imp
                        addedImports += firstAddImport

                        val fullName = s"${groupName}_${boName}"
                        val res = (outFilePath / "scala" / "container" / s"${fullName}_Container_Shares.scala").createFile(failIfExists = false).toFile


                        val mainObjectShared = new ScalaClassDeclare {
                            scalaClassGen = (s"${boName.capitalize}Container_Shared").cls
                            typeScalaClass = TypeScalaObject
                        }

                        for (mode <- operationTypes; _ <- (dataSource \ (mode + "DataURL"))) {
                            val url = s"logic/$fullName/$mode"
                            val urlVar = ScalaVariable(name = s"${fullName}_$mode", serrializeToOneString = true, body = url.dblQuoted.body)

                            mainObjectShared addMembers(newLine, urlVar)
                        }


                        val moduleShared = ScalaModule(
                            s"$packageName.${groupName}Shared".pkg
                        )
                        
                        moduleShared += mainObjectShared

                        res <== {
                            out =>
                                out(genMessageCreating(s"GenBOContainer, stage: $stage"))
                                out(newLine)
                                out(newLine)
                                out(org.scalafmt.Scalafmt.format(moduleShared.serrialize()).get)
                        }

                        resSeq += res
                    }

                }
        }

        resSeq
    }

}
