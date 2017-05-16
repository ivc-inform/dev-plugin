package ru.simplesys.plugins.sourcegen.app.Gen

import java.net.URI

import com.simplesys.common.Strings.{newLine, _}
import com.simplesys.common._
import com.simplesys.genSources._
import com.simplesys.io._
import com.simplesys.javaScriptGen.{JavaScriptBody, JavaScriptSwitch, JavaScriptSwitchCaseItem}
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

                            (collectionElem exists { case (name, _) => name == id.value}) match {
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


    def create: File = ????

    def createSeq: Seq[File] = {
        val resSeq = ArrayBuffer.empty[File]

        val editorsTempl = JavaScriptSwitch(
            switch = "itemName",
            default = JavaScriptBody("return undefined;")
        )

        val dataSourceLoader = ArrayBuffer.empty[String]

        val editors = new ScalaClassDeclare {
            scalaClassGen = "Editors".cls
            typeScalaClass = TypeScalaTrait
        }

        val generatedDataSources = new ScalaClassDeclare {
            scalaClassGen = "GeneratedDataSources".cls
            extensibleClass = "Component".ext
        }

        val dss = ArrayBuffer.empty[(String, ScalaElement)]
        val createScriptsBody = ScalaBody(
            "printFiles1(",
            "  new SimpleTypes,",
            "  new MainTabs,",
            "  new DefaultDataSources,",
            "  new GeneratedDataSources"
        )

        generetedFiles foreach {
            file =>

                val mns = ArrayBuffer.empty[ScalaElement]
                val cnvs = ArrayBuffer.empty[ScalaElement]

                val root: IscElem = loadFile(file.toFile, schemaPath)

                val boName = file.onlyFileName

                val jsBody = JavaScriptBody()

                jsBody += s"return isc.Editor${boName}.create();"

                editorsTempl += JavaScriptSwitchCaseItem(`case` = boName.unCapitalize, body = jsBody)

                for (xmlPiece <- root.child) {
                    val pane: IscElem = xmlPiece
                    //pane.log

                    pane.label match {
                        case "DataSources" =>
                            val dataSourcesString = makeCollectionISCElements(pane, dss)
                            dataSourceLoader += dataSourcesString

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

                val componentClass = new ScalaClassDeclare {
                    scalaClassGen = boName.cls
                    members = mns ++ cnvs
                    extensibleClass = "Component".ext
                }

                createScriptsBody += s"  , new ${boName}"

                val createBody = ScalaBody()
                mns.foreach {
                    _ match {
                        case v: ScalaVariable =>
                            createBody += s"_Isc.componentEnqueue(${v.name}, (${v.name.dblQuoted}, false))"
                            createBody += newLine
                        case _ =>
                    }
                }

                cnvs.foreach {
                    _ match {
                        case v: ScalaVariable =>
                            createBody += s"_Isc.defineClass(${v.name.capitalize.dblQuoted}, ModeNames + ${v.name}.selfName).addClassProperties(${v.name}.json)"
                            createBody += newLine
                        case _ =>
                    }
                }

                createBody += "_Isc.getCommandLine"

                componentClass addMember newLine
                componentClass addMember ScalaMethod(name = "create", `override` = OverrideMofificator, `type` = ScalaString, body = createBody)

                val module = new ScalaModule(
                    (packageName + ".jsGen").pkg,
                    "com.simplesys.isc.system.ArrayDyn".imp,
                    "com.simplesys.isc.dataBinging.{SortSpecifierDyn, DataSourceFieldDyn, RestDataSourceDyn}".imp,
                    "com.simplesys.isc.system.typesDyn._".imp,
                    "com.simplesys.isc.system.misc._".imp,
                    "com.simplesys.isc.dataBinging.RestDataSourceDyn._".imp,
                    "com.simplesys.isc.dataBinging.dataSource.OperationBindingDyn".imp,
                    "com.simplesys.isc.control.{MenuSSDyn, MenuDyn}".imp,
                    "com.simplesys.isc.control.menu.MenuItemDyn".imp,
                    "com.simplesys.components.Dyn._".imp,
                    "com.simplesys.components.Dyn.Layout._".imp,
                    "com.simplesys.isc.grids.treeGrid.TreeGridFieldDyn".imp,
                    "com.simplesys.config.Config".imp,
                    "com.simplesys.isc.control.MenuDyn._".imp,
                    "com.simplesys.json.JsonList".imp,
                    "com.simplesys.isc.forms.DynamicFormDyn".imp,
                    "com.simplesys.isc.forms.DynamicFormDyn._".imp,
                    "com.simplesys.isc.layout.TabSetDyn".imp,
                    "com.simplesys.isc.layout.tabSet.TabDyn".imp,
                    "com.simplesys.isc.grids.ListGridDyn".imp,
                    "com.simplesys.isc.foundation.CanvasDyn".imp,
                    "com.simplesys.isc.foundation.CanvasDyn._".imp,
                    "com.simplesys.components.Component".imp,
                    "com.simplesys.isc.grids.listGrid.ListGridFieldDyn".imp,
                    "com.simplesys.isc.system.global._".imp,
                    "com.simplesys.isc.forms.formItems.FormItemDyn".imp,
                    newLine,
                    componentClass
                )

                val outFile: Path = outFilePath / (boName + ".scala")
                val res = outFile.createFile(failIfExists = false).toFile

                //logger debug module.serrialize()
                //logger debug s"Write to file: ${res.getAbsolutePath}"

                res <== {
                    out =>
                        out(genMessageCreating("GenScalaApp"))
                        out(newLine)
                        out(module.serrialize())
                }

                resSeq += res
        }

        editors addMember "this: MainTabs =>"
        editors addMember newLine
        editors addMember ScalaVariable(name = "getEditor", `lazy` = true, body = ScalaBody("_Isc.declareJSFunction(this, \"getEditor\", s\"{${templ}}\", false, \"itemName\")"), serrializeToOneString = true)
        editors addMember newLine
        editors addMember ScalaVariable(name = "templ", modificator = PrivateMofificator, `lazy` = true, body = ScalaBody(editorsTempl.serrializeSTriplesWrap(4)), serrializeToOneString = false)
        editors addMember newLine
        //editors addMember ScalaVariable(name = "dataSourceLoader", `lazy` = true, body = ScalaBody(s"isc.DataSourceSS.load(${dataSourceLoader.mkString("[", ",".space, "]")})".trplQuotWrp), serrializeToOneString = true)

        //editors setTraits traits

        val module = new ScalaModule(
            (packageName + ".jsGen").pkg,
            "com.simplesys.app.jsGen.MainTabs".imp,
            newLine,
            editors
        )

        val outFile: Path = outFilePath / "Editors.scala"
        val res = outFile.createFile(failIfExists = false).toFile

        res <== {
            out =>
                out(genMessageCreating("GenScalaApp"))
                out(newLine)
                out(module.serrialize())
        }

        generatedDataSources ++= dss.map { case (_, scalaElement) => scalaElement}

        generatedDataSources addMember ScalaMethod(name = "create", `type` = ScalaString, body = ScalaBody(
            s"_Isc.componentsEnqueue(${
                (dss.map {
                    case (name, sclaElement: ScalaVariable) => name
                    case _ => strEmpty
                }).filter(_ != strEmpty).mkString(", ")
            })",
            newLine,
            "_Isc getCommandLine"
        ))

        val moduleDefaultDataSources = new ScalaModule(
            (packageName + ".jsGen").pkg,
            newLine,
            "com.simplesys.isc.dataBinging.{WildRecordColumnDyn, DataSourceFieldDyn}".imp,
            "com.simplesys.isc.system.ArrayDyn".imp,
            "com.simplesys.components.Dyn.DataBinding.RestDataSourceSSDyn".imp,
            "com.simplesys.isc.system.typesDyn._".imp,
            "com.simplesys.isc.dataBinging.dataSource.OperationBindingDyn".imp,
            "com.simplesys.isc.system.misc._".imp,
            "com.simplesys.components.Component".imp,
            newLine,
            generatedDataSources
        )

        val outFileDefaultDataSources: Path = outFilePath / "GeneratedDataSources.scala"
        val resDefaultDataSources = outFileDefaultDataSources.createFile(failIfExists = false).toFile

        resDefaultDataSources <== {
            out =>
                out(genMessageCreating("GenScalaApp (createSeq)"))
                out(newLine)
                out(moduleDefaultDataSources.serrialize())
        }

        val createStaticPath: Path = outFilePath / "CreateStatics.scala"
        val createStaticFile = createStaticPath.toFile

        createScriptsBody += ")"

        val createStatics = new ScalaClassDeclare {
            scalaClassGen = "CreateStatics".cls
            parametrs = ScalaClassParametrs(
                ScalaClassParametr(name = "path", `type` = "File".tp, parametrType = ParametrVal),
                ScalaClassParametr(name = "withBeautifiler", `type` = ScalaBoolean, parametrType = ParametrVal)
            )
            extensibleClass = "PublicStatic".ext
            members = ArrayBuffer(
                ScalaMethod(name = "createScripts", `type` = ScalaUnit, body = createScriptsBody)
            )
            mainConstructorBody = ScalaBody("createScripts")
        }

        val moduleCreateStatics = new ScalaModule(
            "com.simplesys.mfms".pkg,
            "java.io.File".imp,
            newLine,
            "com.simplesys.app.jsGen.MainTabs".imp,
            "com.simplesys.components.Gen.PublicStatic".imp,
            "com.simplesys.isc.system.IscDyn".imp,
            "com.simplesys.isc.system.global._".imp,
            "com.simplesys.mfms.jsGen.DefaultDataSources".imp,
            "ru.simplesys.defs.app.gen.scala.jsGen._".imp,
            "ru.simplesys.defs.app.scala.jsGen.SimpleTypes".imp,
            newLine,
            createStatics
        )

        createStaticFile <== {
            out =>
                out(genMessageCreating("GenScalaApp (createSeq)"))
                out(newLine)
                out(moduleCreateStatics.serrialize())
        }

        resSeq ++= Seq(res, resDefaultDataSources, createStaticFile)
    }
}
