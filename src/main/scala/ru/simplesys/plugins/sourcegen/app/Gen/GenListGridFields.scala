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

class GenListGridFields(val appFilePath: Path,
                        val schemaPath: URI,
                        val outFilePath: Path,
                        val packageName: String,
                        val stage: String,
                        val logger: Logger) extends GenScala1 {

    val generetedFiles: PathSet[Path] = appFilePath * "dataSources.xml"
    type CollectionElem = ArrayBuffer[(String, ScalaElement)]
    type CollectionElemObject = ArrayBuffer[ScalaObjectElement]
    
    val dateTimeFormat = "dd.MM.yyyy HH:mm:ss".dblQuoted
    val dateFormat = "dd.MM.yyyy".dblQuoted

    private def makeCollectionISCElementsJS(parentElem: IscElem, listGridFieldsCollection: CollectionElem, formItemsCollection: CollectionElem, collectionElemObject: CollectionElemObject) = {
        for (element <- parentElem.child.filter(_.label != "#PCDATA")) {
            val fields: IscElem = element \ "Fields"

            val id: IscElem = element \ "Identifier"
            val nameBase = id.value.replace("DS", "")
            (listGridFieldsCollection exists { case (name, _) => name == id.value }) match {
                case false =>
                    val listGridFieldsArray = ArrayBuffer.empty[ScalaElement]
                    val formItemsArray = ArrayBuffer.empty[ScalaElement]

                    for (elementField <- fields.child) {
                        val _elementField: IscElem = elementField
                        val fieldName = (elementField \ "Name").text

                        val listFridField = new ScalaClassDeclare {
                            scalaClassGen = "ListGridFieldProps".cls
                            typeScalaClass = AnonimousScalaClass
                        }

                        val listFridFieldObjectName = s"${nameBase}${fieldName}_NameStrong"
                        val listFridFieldObject = new ScalaClassDeclare {
                            scalaClassGen = listFridFieldObjectName.cls
                            typeScalaClass = TypeScalaObject
                            extensibleClass = "NameStrong".ext
                            //annotation = ScalaAnnotation("ScalaJSDefined")
                        }
                        listFridFieldObject addMember (ScalaVariable(name = "name", body = s"${fieldName.dblQuoted}".body, serrializeToOneString = true))
                        collectionElemObject += listFridFieldObject

                        listFridField addMember ScalaExpression(s"nameStrong = ${listFridFieldObjectName}.opt")

                        val fieldType = (elementField \ "Type").text

                        val fieldHidden = _elementField.getBooleanValue("Hidden")
                        fieldType match {
                            case "sDescription_SimpleType" =>
                                listFridField addMember ScalaExpression(s"`type` = ${"ListGridFieldType.sCaption_SimpleType.opt"}")
                            case "id_SimpleType" =>
                                listFridField addMember ScalaExpression(s"`type` = ${"ListGridFieldType.id_SimpleType.opt"}")
                            case tp =>
                                listFridField addMember ScalaExpression(s"`type` = ${s"ListGridFieldType.$tp.opt"}")

                        }

                        fieldType match {
                            case "clob_SimpleType" =>
                            case "dDate_SimpleType" =>
                                listFridField addMember ScalaExpression(s"align = Alignment.center.opt")
                                listFridField addMember ScalaExpression(s"format = $dateFormat.opt")
                            case "dDateOptTime_SimpleType" =>
                                listFridField addMember ScalaExpression(s"align = Alignment.center.opt")
                                listFridField addMember ScalaExpression(s"format = $dateTimeFormat.opt")
                            case "dDateTime_SimpleType" =>
                                listFridField addMember ScalaExpression(s"align = Alignment.center.opt")
                                listFridField addMember ScalaExpression(s"format = $dateTimeFormat.opt")
                            case "dTimestamp_SimpleType" =>
                                listFridField addMember ScalaExpression(s"align = Alignment.center.opt")
                                listFridField addMember ScalaExpression(s"format = $dateTimeFormat.opt")
                            case "dTimestampWithTZ_SimpleType" =>
                                listFridField addMember ScalaExpression(s"align = Alignment.center.opt")
                                listFridField addMember ScalaExpression(s"format = $dateTimeFormat.opt")
                            case "di_SimpleType" =>
                            case "fDouble_SimpleType" =>
                            case "fPrice_SimpleType" =>
                            case "fSum_SimpleType" =>
                            case "id_SimpleType" =>
                            case "nInt_SimpleType" =>
                            case "sAddress_SimpleType" =>
                            case "sBarCode_SimpleType" =>
                            case "sCaption_SimpleType" =>
                            case "sCode_SimpleType" =>
                            case "sDescription_SimpleType" =>
                            case "sEMail_SimpleType" =>
                            case "sPasswordHashSHA_SimpleType" =>
                            case "sPasswordPlain_SimpleType" =>
                            case "sPhone_SimpleType" =>
                            case "sPostalIndex_SimpleType" =>
                            case "sURL_SimpleType" =>
                            case "sURLDomain_SimpleType" =>
                            case "sURLImage_SimpleType" =>
                            case "ss_SimpleType" =>
                            case "bBoolean_SimpleType" =>
                            case "blob_SimpleType" =>
                            case "json_SimpleType" =>
                            case _ => "TextItem"
                        }

                        if (fieldHidden)
                            listFridField addMember ScalaExpression(s" hidden = true")

                        listGridFieldsArray += listFridField

                        val formItem = new ScalaClassDeclare {
                            scalaClassGen = s"FormItemProps".cls
                            wrappadOperator = "FormItem"
                            typeScalaClass = AnonimousScalaClass
                        }

                        fieldType match {
                            case "clob_SimpleType" =>
                            case "dDate_SimpleType" =>
                                formItem addMember ScalaExpression(s"align = Alignment.center.opt")
                                formItem addMember ScalaExpression(s"format = $dateTimeFormat.opt")
                            case "dDateOptTime_SimpleType" =>
                                formItem addMember ScalaExpression(s"align = Alignment.center.opt")
                                formItem addMember ScalaExpression(s"format = $dateTimeFormat.opt")
                            case "dDateTime_SimpleType" =>
                                formItem addMember ScalaExpression(s"align = Alignment.center.opt")
                                formItem addMember ScalaExpression(s"format = $dateTimeFormat.opt")
                            case "dTimestamp_SimpleType" =>
                                formItem addMember ScalaExpression(s"align = Alignment.center.opt")
                                formItem addMember ScalaExpression(s"format = $dateTimeFormat.opt")
                            case "dTimestampWithTZ_SimpleType" =>
                                formItem addMember ScalaExpression(s"align = Alignment.center.opt")
                                formItem addMember ScalaExpression(s"format = $dateTimeFormat.opt")
                            case "di_SimpleType" =>
                            case "fDouble_SimpleType" =>
                            case "fPrice_SimpleType" =>
                            case "fSum_SimpleType" =>
                            case "id_SimpleType" =>
                            case "nInt_SimpleType" =>
                            case "sAddress_SimpleType" =>
                            case "sBarCode_SimpleType" =>
                            case "sCaption_SimpleType" =>
                            case "sCode_SimpleType" =>
                            case "sDescription_SimpleType" =>
                            case "sEMail_SimpleType" =>
                            case "sPasswordHashSHA_SimpleType" =>
                            case "sPasswordPlain_SimpleType" =>
                            case "sPhone_SimpleType" =>
                            case "sPostalIndex_SimpleType" =>
                            case "sURL_SimpleType" =>
                            case "sURLDomain_SimpleType" =>
                            case "sURLImage_SimpleType" =>
                            case "ss_SimpleType" =>
                            case "bBoolean_SimpleType" =>
                            case "blob_SimpleType" =>
                            case "json_SimpleType" =>
                            case _ => "TextItem"
                        }

                        formItem addMember ScalaExpression(s"nameStrong = ${listFridFieldObjectName}.opt")
                        formItem addMember ScalaExpression(s"`type` = ${s"FormItemType.$fieldType"}")
                        if (fieldHidden)
                            formItem addMember ScalaExpression(s" hidden = true")
                        formItemsArray += formItem
                    }

                    listGridFieldsCollection += Tuple2(id.value, ScalaVariable(
                        name = s"${nameBase}FLDS",
                        body = ScalaBody(ScalaSequense(listGridFieldsArray: _*))
                    ))
                    listGridFieldsCollection += Tuple2(id.value + "newLine1", newLine)

                    formItemsCollection += Tuple2(id.value, ScalaVariable(
                        name = s"${nameBase}FRMITM",
                        body = ScalaBody(ScalaSequense(formItemsArray: _*))
                    ))
                    formItemsCollection += Tuple2(id.value + "newLine1", newLine)
                case _ =>
            }
        }
    }

    def create: File = ????

    def createSeq: Seq[File] = {
        val resSeq = ArrayBuffer.empty[File]

        val listGridFields = new ScalaClassDeclare {
            scalaClassGen = "ListGridFiledsJS".cls
            typeScalaClass = TypeScalaObject
            extensibleClass = "Implicits".ext
        }

        val listFormItems = new ScalaClassDeclare {
            scalaClassGen = "FormItemsJS".cls
            typeScalaClass = TypeScalaObject
            extensibleClass = "Implicits".ext
        }

        val lss = ArrayBuffer.empty[(String, ScalaElement)]
        val lsf = ArrayBuffer.empty[(String, ScalaElement)]
        val lssNames = ArrayBuffer.empty[ScalaObjectElement]

        generetedFiles foreach {
            file =>
                val root: IscElem = loadFile(file.toFile, schemaPath)

                root.label match {
                    case "DataSources" =>
                        makeCollectionISCElementsJS(root, lss, lsf, lssNames)

                    case label =>
                        throw new RuntimeException(s"Unknown implemantation for root.label : ${label.dblQuoted}")
                }
        }

        listGridFields ++= lss.map { case (_, scalaElement) => scalaElement }
        listFormItems ++= lsf.map { case (_, scalaElement) => scalaElement }

        val moduleDataSourcesJS = new ScalaModule(
            (packageName + ".ScalaJSGen").pkg,
            newLine,
            "com.simplesys.SmartClient.Forms.formsItems.props.FormItemProps".imp,
            "com.simplesys.SmartClient.Grids.props.listGrid.ListGridFieldProps".imp,
            "com.simplesys.SmartClient.System.{FormItem, Implicits}".imp,
            "com.simplesys.System.Types.{Alignment, FormItemType, ListGridFieldType}".imp,
            "com.simplesys.option.ScOption._".imp,
            "com.simplesys.System.NameStrong".imp,
            "com.simplesys.System.Types.Alignment._".imp,
            //"scala.scalajs.js.annotation.ScalaJSDefined".imp,
            newLine
        )

        moduleDataSourcesJS ++= lssNames.toArray
        moduleDataSourcesJS += newLine
        moduleDataSourcesJS += listGridFields
        moduleDataSourcesJS += newLine
        moduleDataSourcesJS += listFormItems

        val outFileDataSourcesJS: Path = outFilePath / "ListGridFiledsJS.scala"
        val resDataSourcesJS = outFileDataSourcesJS.createFile(failIfExists = false).toFile

        resDataSourcesJS <== {
            out =>
                out(genMessageCreating(s"GenScalaApp (createSeq), stage: $stage"))
                out(newLine)
                out(moduleDataSourcesJS.serrialize())
        }

        resSeq += resDataSourcesJS
    }
}
