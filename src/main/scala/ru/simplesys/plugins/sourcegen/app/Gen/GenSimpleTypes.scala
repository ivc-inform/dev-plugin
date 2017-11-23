package ru.simplesys.plugins.sourcegen.app.Gen

import sbt.{Logger, File}
import com.simplesys.common.Strings.newLine
import scalax.file.Path
import com.simplesys.io._
import com.simplesys.genSources._
import com.simplesys.scalaGen._
import java.net.URI
import ru.simplesys.plugins.sourcegen.app.xml.IscElem
import com.simplesys.xhtml.XHTML._
import com.simplesys.common.Strings._

class GenSimpleTypes(val appFilePath: Path,
                     val schemaPath: URI,
                     val outFilePath: Path,
                     val packageName: String,
                     val stage:String,
                     val logger: Logger) extends GenScala {

    override def create: File = {
        val root: IscElem = loadFile(appFilePath.toFile, schemaPath)

        val simpleTypes = new ScalaClassDeclare {
            extensibleClass = "Component".ext
            scalaClassGen = "SimpleTypes".cls
        }

        val createMethodBody = ScalaBody()

        val createMethod = ScalaMethod(
            name = "create",
            body = createMethodBody,
            `type` = ScalaString
        )

        for (xmlPiece <- root.child) {
            val simpleType: IscElem = xmlPiece
            simpleType.label match {
                case "SimpleTypeDyn" =>
                    val elem = ((simpleType \ "Name"):IscElem).value
                    createMethodBody += ScalaVariable(name = elem, body = makeScalaCode(simpleType))
                    createMethodBody ++= (
                        s"_Isc componentEnqueue ${elem}",
                        newLine
                      )
                case label =>
                    throw new RuntimeException(s"Unknown implemantation for simpleType.label : ${label.dblQuoted}")
            }
        }

        createMethodBody += "_Isc getCommandLine"

        simpleTypes addMember createMethod

        val module = new ScalaModule(
            (packageName + ".jsGen").pkg,
            "com.simplesys.components.Component".imp,
            "com.simplesys.isc.dataBinging.{DataSourceFieldDyn, SimpleTypeDyn}".imp,
            "com.simplesys.json.JsonList".imp,
            "com.simplesys.isc.system.typesDyn._".imp,
            "com.simplesys.isc._".imp,
            newLine,
            simpleTypes
        )

        val res = outFilePath.createFile(failIfExists = false).toFile

        res <== {
            out =>
                out(genMessageCreating(s"GenSimpleTypes, stage: $stage"))
                out(newLine)
                out(org.scalafmt.Scalafmt.format(module.serrialize()).get)
        }
    }
}
