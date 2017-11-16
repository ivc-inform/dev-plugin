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
import ru.simplesys.plugins.sourcegen.app.{AttrDefOrd, ForeignKeyConstraintDefOrd}
import ru.simplesys.plugins.sourcegen.meta._
import sbt.{File, Logger}

import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting

class GenBOServlet(val appFilePath: Path,
                   val boFilePath: Path,
                   val schemaPath: URI,
                   val outFilePath: Path,
                   val packageName: String,
                   val pkgBOName: String,
                   val stage: String,
                   val logger: Logger) extends GenScala1 {

    val operationTypes = Seq("Add", "Fetch", "Remove", "Update")
    val sourceBOFiles: PathSet[Path] = boFilePath * "*.xml"
    val generetedFiles: PathSet[Path] = appFilePath * "*.xml"


    def create: File = ????

    def createSeq: Seq[File] = {
        implicit val schema = SchemaDef(pkgBOName, sourceBOFiles.files)

        val resSeq = ArrayBuffer.empty[File]
        val servletes = ArrayBuffer.empty[String]

        generetedFiles foreach {
            file =>
                val root: IscElem = loadFile(file.toFile, schemaPath)
                val dataSources: IscElem = root \ "DataSources"

                for (_dataSource <- dataSources.child) {
                    val classServletes = ArrayBuffer.empty[ScalaObjectElement]
                    val addedImports = ScalaImports()

                    val dataSource: IscElem = _dataSource
                    val dataSourceIdentifier = (dataSource \ "Identifier").text.replace("_DS", "")

                    if (dataSourceIdentifier.isEmpty)
                        throw new RuntimeException("Не определен Identifier для DataSource.")

                    if (!servletes.exists(_ === dataSourceIdentifier)) {

                        servletes += dataSourceIdentifier
                        val boName = dataSourceIdentifier.substring(dataSourceIdentifier.indexOf("_") + 1)
                        val groupName = dataSourceIdentifier.substring(0, dataSourceIdentifier.indexOf("_"))

                        addedImports += (s"${pkgBOName}.${groupName}._").imp

                        val fullName = s"${groupName}_${boName}"
                        val res = (outFilePath / "scala" / "servlet" / (fullName + "_Sevlet.scala")).createFile(failIfExists = false).toFile

                        val dataURL = dataSource getStringValue "DataURL"
                        val _dataURL: String = {
                            var res = ""
                            if (dataURL.head != '/')
                                res += "/"
                            if (dataURL.indexOf("@") != -1)
                                res += dataURL.substring(0, dataURL.indexOf("@"))
                            else
                                res += dataURL
                            res.dblQuoted
                        }
                        val classServletAnnotation = ScalaAnnotation("WebServlet", "asyncSupported" -> "true", "urlPatterns" -> ("Array(" + _dataURL + ")"))

                        val classServlet = new ScalaClassDeclare {
                            annotation = classServletAnnotation
                            extensibleClass = ScalaClassAkkaHttpPartialServlet
                            scalaClassGen = (fullName + "_Servlet").cls
                            withTraits = ScalaWithTraits(ScalaTraitStaticEndpoints)
                        }

                        val actorClasses = ArrayBuffer.empty[ScalaObjectElement]

                        val onSystemInitMethodBody = ScalaBody(
                            "actorSystem = system"
                        )

                        val providersPartionalMethodBody = ScalaCase()
                        val suitTraitMethods = ArrayBuffer.empty[ScalaElement]

                        //<editor-fold desc="Переменные типа private [this] var actor???:ActorRef = null">
                        classServlet addMember ScalaVariable(name = "actorSystem", modificator = PrivateMofificator, packageScope = "this", variableType = VariableVar, `type` = "ActorSystem".tp, body = "_".body, serrializeToOneString = true)
                        classServlet addMember newLine

                        def actorName(mode: String) = s"${fullName}_${mode}Actor"

                        onSystemInitMethodBody += newLine
                        for (mode <- operationTypes; _dataURL <- (dataSource \ (mode + "DataURL"))) {
                            classServlet addMember ScalaVariable(name = "actor" + mode, modificator = PrivateMofificator, packageScope = "this", variableType = VariableVar, `type` = "ActorRef".tp, body = "_".body, serrializeToOneString = true)
                            onSystemInitMethodBody += s"actor${mode} = actorSystem.actorOf(${actorName(mode)}.props)"
                        }
                        onSystemInitMethodBody += newLine
                        onSystemInitMethodBody += "super.onSystemInit(system, endpoints)"
                        //</editor-fold>

                        for (mode <- operationTypes; _dataURL <- (dataSource \ (mode + "DataURL"))) {


                            //<editor-fold desc=" Body Метода providersPartial">
                            providersPartionalMethodBody += ScalaCaseLine(s"/logic/${fullName}/${mode}".dblQuoted.expr, serrializeToOneString = true, caseBody = s"actor${mode}".body)
                            //</editor-fold>

                            // <editor-fold desc="Классы типа class ????ХХХХХActor extends Actor with Suite????">
                            val actorClass = new ScalaClassDeclare {
                                scalaClassGen = actorName(mode).cls
                                extensibleClass = ScalaClassActor
                                withTraits = ScalaWithTraits(s"Suit_${fullName}".trt)
                            }

                            val actorObject = new ScalaClassDeclare {
                                scalaClassGen = actorName(mode).cls
                                typeScalaClass = TypeScalaObject
                            }

                            actorObject addMember ScalaMethod(name = "props", serrializeToOneString = true, body = s"Props(new ${actorName(mode)})".body)

                            val suitTraitMethodName = mode.unCapitalize + "Data"
                            actorClass addMember ScalaPartialMethod(
                                name = "receive",
                                body = ScalaCase(ScalaCaseLine(
                                    expression = "request".expr,
                                    `type` = ScalaHttpServletRequest,
                                    caseBody = ScalaBody(
                                        ScalaVariable(name = "future", serrializeToOneString = true, body = s"FutureResponse {response => ${suitTraitMethodName}(request, response)(context.system)}".body),
                                        newLine,
                                        "sender ! Complete(future)",
                                        newLine
                                    ))
                                ))

                            actorClasses ++= Seq(actorObject, actorClass)

                            // </editor-fold>

                            //<editor-fold desc="Методы Suit??? Trait">

                            suitTraitMethods += {

                                def recordDyn(itemName: String, boName: String = strEmpty) = ScalaApplyObject(name = "RecordDyn",
                                    parametrs =
                                      ScalaClassParametrs(
                                          (_dataSource \ "Fields" \ "DataSourceFieldDyn") map {
                                              x =>
                                                  val name = (x: IscElem).getStringValue("Name")
                                                  val jObjectFieldName = (x: IscElem).getStringValue("JObjectFieldName")
                                                  val lookup = (x: IscElem).getBooleanValue("Lookup")
                                                  val getterType = (x: IscElem).getStringValue("GetterType")
                                                  val _boName = jObjectFieldName.substring(jObjectFieldName.indexOf(".") + 1) + jObjectFieldName.substring(0, jObjectFieldName.indexOf(".")).capitalize

                                                  if (itemName == strEmpty)
                                                      ScalaClassParametr(name = name.dblQuoted, `type` = ScalaImplicitType, defaultValue = _boName, sign = ScalaSignArrowRight)
                                                  else if (!lookup)
                                                      ScalaClassParametr(name = name.dblQuoted, `type` = ScalaImplicitType, defaultValue = {
                                                          if (itemName.isEmpty) s"${name}" else s"${itemName}.${name}"
                                                      } + boName, sign = ScalaSignArrowRight)
                                                  else
                                                      ScalaClassParametr(name = name.dblQuoted, `type` = ScalaImplicitType, defaultValue = s"data.get${getterType}(${name.dblQuoted})", sign = ScalaSignArrowRight)
                                          }: _*
                                      )
                                )

                                def tuple: ScalaApplyObject = {
                                    var i = 0
                                    val allColumns = ArrayBuffer.empty[String]

                                    val clazz = schema.resolveClass(LinkRefToClassOld(groupName = groupName, objectName = boName))
                                    val attrs: Array[AttrDef[_]] = clazz.attrs.toArray
                                    Sorting.quickSort(attrs)(AttrDefOrd)

                                    def getParams(attrs: Seq[AttrDef[_]], clazz: IClass): Seq[ScalaClassParametr] = {
                                        val res = ArrayBuffer.empty[ScalaClassParametr]
                                        attrs foreach {
                                            attr =>
                                                val fieldName = s"${attr.name}${clazz.className.capitalize}"

                                                val _columnType = {
                                                    val res = attr.attrType.scalaTypeAsString(clazz.group, schema)
                                                    if (res.indexOf(".") !== -1) {
                                                        addedImports += res.imp
                                                        logger debug (s"Bad type: ${res.dblQuoted} tarnsform to ${res.substring(res.lastIndexOf(".") + 1).dblQuoted} and added import ${res}")

                                                        res.substring(res.lastIndexOf(".") + 1)
                                                    } else
                                                        res
                                                }

                                                def columnType = s"${if (attr.isMandatory) _columnType else s"Array[${_columnType}]"}"

                                                if (i < maxArity) {
                                                    if (!allColumns.exists(_.trim === fieldName)) {
                                                        res += ScalaClassParametr(name = fieldName, `type` = columnType.tp)
                                                        allColumns += fieldName.space
                                                        i += 1
                                                    }
                                                }
                                        }
                                        res.toSeq
                                    }

                                    val params = ScalaClassParametrs()
                                    params ++= (getParams(attrs, clazz): _*)

                                    val fks = clazz.fks.toArray
                                    //val fks = clazz.fks

                                    Sorting.quickSort(fks)(ForeignKeyConstraintDefOrd)
                                    fks foreach {
                                        case fk: ForeignKeyConstraintDef =>
                                            val fkClass = fk.referencedClassRef.toClass
                                            if (fkClass.className != clazz.className || fkClass.group != clazz.group) {

                                                val attrs1: Array[AttrDef[_]] = fkClass.pk.attrs.toArray
                                                Sorting.quickSort(attrs1)(AttrDefOrd)
                                                params ++= (getParams(attrs1, fkClass): _*)

                                                val attrs2: Array[AttrDef[_]] = fkClass.defaultSettings.showAttrsResult.toArray
                                                Sorting.quickSort(attrs2)(AttrDefOrd)
                                                params ++= (getParams(attrs2, fkClass): _*)
                                            }
                                    }

                                    ScalaApplyObject(name = "TupleSS" + params.length.toString, parametrs = params)
                                }

                                def boData(newLine: Boolean): ScalaApplyObject = {
                                    val parametrs = ArrayBuffer.empty[ScalaClassParametr]

                                    (_dataSource \ "Fields" \ "DataSourceFieldDyn").filter(x => (x: IscElem).getBooleanValue("Calculated") == false && (x: IscElem).getBooleanValue("Discriminator") == false) foreach {
                                        x =>
                                            val name = (x: IscElem).getStringValue("Name")
                                            val getterType = (x: IscElem).getStringValue("GetterType")
                                            val lookup = (x: IscElem).getBooleanValue("Lookup")
                                            val genBySeq = (x: IscElem).getBooleanValue("GenBySeq")

                                            if (!lookup)
                                                if (!newLine)
                                                    parametrs += ScalaClassParametr(name = name, `type` = ScalaImplicitType, defaultValue = s"data.get${getterType}(${name.dblQuoted})")
                                                else
                                                    genBySeq match {
                                                        case false =>
                                                            parametrs += ScalaClassParametr(name = name, `type` = ScalaImplicitType, defaultValue = s"data.get${getterType}(${name.dblQuoted})")
                                                        case true =>
                                                            getterType match {
                                                                case "Long" =>
                                                                    parametrs += ScalaClassParametr(name = name, `type` = ScalaImplicitType, defaultValue = "Sequences(ds).nextLong1(dataSet.fromBO.fromTable.databaseTablename)")
                                                                case "Double" =>
                                                                    parametrs += ScalaClassParametr(name = name, `type` = ScalaImplicitType, defaultValue = "Sequences(ds).nextDouble1(dataSet.fromBO.fromTable.databaseTablename)")
                                                                case "BigDecimal" =>
                                                                    parametrs += ScalaClassParametr(name = name, `type` = ScalaImplicitType, defaultValue = "Sequences(ds).nextBigDecimal1(dataSet.fromBO.fromTable.databaseTablename)")
                                                                case "String" =>
                                                                    parametrs += ScalaClassParametr(name = name, `type` = ScalaImplicitType, defaultValue = "getGUID")
                                                                case x =>
                                                                    throw new RuntimeException(s"GenBySeq must be BigDecimal or Long or String, but exist (${x})")
                                                            }
                                                    }
                                    }

                                    ScalaApplyObject(
                                        name = boName.capitalize,
                                        parametrs =
                                          ScalaClassParametrs(
                                              serrializeToOneString = false,
                                              indent = boName.length + 1,
                                              parametrs: _*
                                          )
                                    )
                                }

                                case class PkData(getterType: String, pk: String)

                                def getPkField: Seq[PkData] = {
                                    val res = (_dataSource \ "Fields" \ "DataSourceFieldDyn").filter(x => (x: IscElem).getBooleanValue("PrimaryKey") == true) map {
                                        node =>
                                            val name = (node: IscElem).getStringValue("Name")
                                            val getterType = (node: IscElem).getStringValue("GetterType")

                                            PkData(getterType, s"${name}")
                                    }
                                    res
                                }

                                val _body: Body = mode match {

                                    case "Add" =>
                                        addedImports += s"${pkgBOName}.${groupName}.${boName.capitalize}".imp

                                        ScalaBody(
                                            ScalaVariable(name = "requestData", body = "new DSRequestDyn(request)".body, serrializeToOneString = true),
                                            ScalaVariable(name = "transaction", body = "requestData.Transaction".body, serrializeToOneString = true),
                                            newLine,
                                            ScalaVariable(name = "sessionContext", body = "new SessionContext(request.Session)".body, serrializeToOneString = true),
                                            ScalaVariable(name = "ds", body = "sessionContext.getDS".body, serrializeToOneString = true, `implicit` = true),
                                            ScalaVariable(name = "dataSet", body = ScalaBody( s"""new ${boName.capitalize}DS"""), serrializeToOneString = true),
                                            newLine,
                                            ScalaMethod(name = "recRequestAddData",
                                                parametrs = ScalaClassParametrs(
                                                    ScalaClassParametr(name = "operation", `type` = "DSRequestDyn".tp),
                                                    ScalaClassParametr(name = "_queueStatus", `type` = "Number".tp)
                                                ),
                                                `type` = "DSResponseDyn".tp,
                                                body = ScalaBody(
                                                    ScalaVariable(name = "data", body = "operation.Data".body, serrializeToOneString = true),
                                                    "logger trace (s\"data: ${newLine + data.toPrettyString}\")",
                                                    newLine,
                                                    ScalaVariable(name = s"${boName}Data".unCapitalize, body = ScalaBody(boData(true))),
                                                    newLine,
                                                    ScalaVariable(name = "insert", body = s"dataSet.insertP(${boName.unCapitalize}Data)".body, serrializeToOneString = true),
                                                    newLine,
                                                    ScalaCase(expression = "insert result".expr,
                                                        ScalaCaseLine(expression = "Success(res)".expr,
                                                            caseBody = ScalaBody(
                                                                "res foreach (x => logger trace (s\"Inserted: ${x} line(s).\"))",
                                                                newLine,
                                                                ScalaVariable(name = "resp",
                                                                    body = ScalaBody(
                                                                        {
                                                                            val res = new ScalaClassDeclare {
                                                                                scalaClassGen = "DSResponseDyn".cls
                                                                                typeScalaClass = AnonimousScalaClass
                                                                            }
                                                                            res addMember ("Status = RPCResponseDyn.statusSuccess")

                                                                            res addMember (ScalaVariable(name = "Data", body = ScalaBody(recordDyn(s"${boName}Data".unCapitalize)), serrializeToOneString = true, variableType = AssignVariable))

                                                                            res
                                                                        }
                                                                    ),
                                                                    serrializeToOneString = false
                                                                ),
                                                                "resp"
                                                            )
                                                        ),
                                                        ScalaCaseLine(expression = "Failure(_)".expr,
                                                            caseBody = ScalaBody("new DSResponseFailureExDyn(insert)")
                                                        )
                                                    )
                                                )
                                            ),
                                            newLine,
                                            "makeResponce(transaction, sessionContext, recRequestAddData, response, requestData)"
                                        )

                                    case "Fetch" =>
                                        addedImports += s"${pkgBOName}.${groupName}.${boName.capitalize}DS".imp

                                        ScalaBody(
                                            ScalaVariable(name = "_data", body = "RecordsDynList()".body, serrializeToOneString = true),
                                            ScalaVariable(name = "sessionContext", body = "new SessionContext(request.Session)".body, serrializeToOneString = true),
                                            newLine,
                                            ScalaVariable(name = "requestData", body = "new DSRequestDyn(request)".body, serrializeToOneString = true),
                                            "logger trace s\"Request for fetch: ${newLine + requestData.toPrettyString}\"",
                                            newLine,
                                            ScalaVariable(name = "transaction", body = "requestData.Transaction".body, serrializeToOneString = true),
                                            "logger trace s\"Transaction for fetch: ${newLine + transaction.toPrettyString}\"",
                                            newLine,
                                            ScalaVariable(name = "ds", body = "sessionContext.getDS".body, serrializeToOneString = true, `implicit` = true),
                                            ScalaVariable(name = "dataSet", body = ScalaBody( s"""new ${boName.capitalize}DS"""), serrializeToOneString = true),
                                            newLine,
                                            ScalaMethod(name = "fetchRequestData",
                                                parametrs = ScalaClassParametrs(
                                                    ScalaClassParametr(name = "operation", `type` = "DSRequestDyn".tp),
                                                    ScalaClassParametr(name = "_queueStatus", `type` = "Number".tp)
                                                ),
                                                `type` = "DSResponseDyn".tp,
                                                body = ScalaBody(
                                                    ScalaVariable(name = "qty", `type` = ScalaInt, body = "operation.EndRow.toInt - operation.StartRow.toInt".body, serrializeToOneString = true),
                                                    newLine,
                                                    ScalaVariable(name = "select", body = ScalaBody(s"dataSet.Fetch(dsRequest = DSRequest(sqlDialect = sessionContext.getSQLDialect, startRow = operation.StartRow, endRow = operation.EndRow, sortBy = operation.SortBy, data = operation.Data, textMatchStyle = operation.TextMatchStyle.toString))"), serrializeToOneString = true),
                                                    newLine,
                                                    ScalaCase(expression = "select.result".expr,
                                                        ScalaCaseLine(expression = "Success(list)".expr,
                                                            caseBody = ScalaBody(
                                                                ScalaControlStruct(name = "list foreach",
                                                                    body =
                                                                      ScalaBody(
                                                                          ScalaCase(
                                                                              ScalaCaseLine(expression = ScalaExpressionBody(ScalaBody(tuple)),
                                                                                  caseBody = ScalaBody(
                                                                                      ScalaVariable(name = "_data", variableType = AssignVariable, sign = "+=",
                                                                                          body = ScalaBody(recordDyn(strEmpty, boName)),
                                                                                          serrializeToOneString = true
                                                                                      )
                                                                                  )
                                                                              )
                                                                          )
                                                                      ),
                                                                    serrializeToOneString = true
                                                                ),
                                                                newLine,
                                                                "logger trace s\"_data: ${newLine + _data.toPrettyString}\"",
                                                                newLine, {
                                                                    val res = new ScalaClassDeclare {
                                                                        scalaClassGen = "DSResponseDyn".cls
                                                                        typeScalaClass = AnonimousScalaClass
                                                                    }
                                                                    res addMembers(
                                                                      "Status = RPCResponseDyn.statusSuccess",
                                                                      "Data = _data",
                                                                      s"TotalRows = operation.StartRow.toInt + (if (qty == list.length) qty + (4 * (operation.EndRow.toInt - operation.StartRow.toInt)) else list.length)"
                                                                    )

                                                                    res
                                                                })
                                                        ),
                                                        ScalaCaseLine(expression = "Failure(_)".expr,
                                                            caseBody = ScalaBody("new DSResponseFailureExDyn(select)")
                                                        )
                                                    ),
                                                    newLine
                                                )
                                            ),
                                            "makeResponce(transaction, sessionContext, fetchRequestData, response, requestData)"
                                        )

                                    case "Update" =>
                                        def getUpdateSatement: ScalaVariable = {
                                            var step = 0
                                            var body = ""

                                            val clazz = schema.resolveClass(LinkRefToClassOld(groupName = groupName, objectName = boName))

                                            clazz.pk.attrNames.foreach {
                                                attr =>
                                                    if (step == 0)
                                                        body = s"dataSet.updateP(${boName.unCapitalize}Data, where = Where(dataSet.${attr}${boName.capitalize} === ${boName.unCapitalize}Data.${attr})"
                                                    else
                                                        body += space + s"And (dataSet.${attr}${boName.capitalize} === ${boName.unCapitalize}Data.${attr})"
                                                    step += 1
                                            }

                                            ScalaVariable(name = "update", body = (body + ")").body, serrializeToOneString = true)
                                        }

                                        ScalaBody(
                                            ScalaVariable(name = "requestData", body = "new DSRequestDyn(request)".body, serrializeToOneString = true),
                                            ScalaVariable(name = "transaction", body = "requestData.Transaction".body, serrializeToOneString = true),
                                            newLine,
                                            "logger trace (s\"Transaction:${newLine + requestData.toPrettyString}\")",
                                            newLine,
                                            ScalaVariable(name = "sessionContext", body = "new SessionContext(request.Session)".body, serrializeToOneString = true),
                                            ScalaVariable(name = "ds", body = "sessionContext.getDS".body, serrializeToOneString = true, `implicit` = true),
                                            ScalaVariable(name = "dataSet", body = ScalaBody( s"""new ${boName.capitalize}DS"""), serrializeToOneString = true),
                                            newLine,
                                            ScalaMethod(name = "recRequestUpdateData",
                                                parametrs = ScalaClassParametrs(
                                                    ScalaClassParametr(name = "operation", `type` = "DSRequestDyn".tp),
                                                    ScalaClassParametr(name = "_queueStatus", `type` = "Number".tp)
                                                ),
                                                `type` = "DSResponseDyn".tp,
                                                body = ScalaBody(
                                                    ScalaVariable(name = "data", body = "operation.OldValues ++ operation.Data".body, serrializeToOneString = true),
                                                    //ScalaVariable(name = "data", body = "operation.Data".body, serrializeToOneString = true),
                                                    "logger trace (s\"data: ${newLine + data.toPrettyString}\")",
                                                    newLine,
                                                    ScalaVariable(name = s"${boName.unCapitalize}Data", body = ScalaBody(boData(false)), serrializeToOneString = false),
                                                    newLine,
                                                    getUpdateSatement,
                                                    newLine,
                                                    ScalaCase(expression = "update result".expr,
                                                        ScalaCaseLine(expression = "Success(res)".expr,
                                                            caseBody = ScalaBody(
                                                                "res foreach (x => logger trace (s\"Updated: ${x} line(s).\"))",
                                                                newLine,
                                                                ScalaVariable(name = "resp",
                                                                    body = ScalaBody(
                                                                        {
                                                                            val res = new ScalaClassDeclare {
                                                                                scalaClassGen = "DSResponseDyn".cls
                                                                                typeScalaClass = AnonimousScalaClass
                                                                            }
                                                                            res addMember ("Status = RPCResponseDyn.statusSuccess")

                                                                            res addMember (ScalaVariable(name = "Data", body = ScalaBody(recordDyn(s"${boName.unCapitalize}Data")), serrializeToOneString = true, variableType = AssignVariable))

                                                                            res
                                                                        }
                                                                    ),
                                                                    serrializeToOneString = false
                                                                ),
                                                                "resp"
                                                            )
                                                        ),
                                                        ScalaCaseLine(expression = "Failure(_)".expr,
                                                            caseBody = ScalaBody("new DSResponseFailureExDyn(update)")
                                                        )
                                                    )
                                                )
                                            ),
                                            newLine,
                                            "makeResponce(transaction, sessionContext, recRequestUpdateData, response, requestData)"
                                        )

                                    case "Remove" =>
                                        def getPrimarykeyVariables: ScalaVariables = {
                                            ScalaVariables(getPkField.map {
                                                case PkData(getterType: String, pk: String) =>
                                                    ScalaVariable(name = pk, body = s"data.get${getterType}(${pk.dblQuoted})".body, serrializeToOneString = true)
                                            }: _*)
                                        }

                                        def getDeleteSatement: ScalaVariable = {
                                            var step = 0
                                            var body = ""

                                            val clazz = schema.resolveClass(LinkRefToClassOld(groupName = groupName, objectName = boName))

                                            clazz.pk.attrNames.foreach {
                                                attr =>
                                                    if (step == 0)
                                                        body = s"dataSet.delete(where = Where(dataSet.${attr}${boName.capitalize} === ${attr})"
                                                    else
                                                        body += space + s"And (dataSet.${attr}${boName.capitalize} === ${attr})"
                                                    step += 1
                                            }

                                            ScalaVariable(name = "delete", body = (body + ")").body, serrializeToOneString = true)
                                        }

                                        ScalaBody(
                                            ScalaVariable(name = "requestData", body = "new DSRequestDyn(request)".body, serrializeToOneString = true),
                                            ScalaVariable(name = "transaction", body = "requestData.Transaction".body, serrializeToOneString = true),
                                            newLine,
                                            ScalaVariable(name = "sessionContext", body = "new SessionContext(request.Session)".body, serrializeToOneString = true),
                                            ScalaVariable(name = "ds", body = "sessionContext.getDS".body, serrializeToOneString = true, `implicit` = true),
                                            ScalaVariable(name = "dataSet", body = ScalaBody( s"""new ${boName.capitalize}DS"""), serrializeToOneString = true),
                                            newLine,
                                            ScalaMethod(name = "recRequestRemoveData",
                                                parametrs = ScalaClassParametrs(
                                                    ScalaClassParametr(name = "operation", `type` = "DSRequestDyn".tp),
                                                    ScalaClassParametr(name = "_queueStatus", `type` = "Number".tp)
                                                ),
                                                `type` = "DSResponseDyn".tp,
                                                body = ScalaBody(
                                                    ScalaVariable(name = "data", body = "operation.Data".body, serrializeToOneString = true),
                                                    "logger trace (s\"data: ${newLine + data.toPrettyString}\")",
                                                    newLine,
                                                    getPrimarykeyVariables,
                                                    getDeleteSatement,
                                                    newLine,
                                                    ScalaCase(expression = "delete result".expr,
                                                        ScalaCaseLine(expression = "Success(res)".expr,
                                                            caseBody = ScalaBody(
                                                                "res foreach (x => logger trace (s\"Deleted: ${x} line(s).\"))",
                                                                newLine,
                                                                ScalaVariable(name = "resp",
                                                                    body = ScalaBody(
                                                                        {
                                                                            val res = new ScalaClassDeclare {
                                                                                scalaClassGen = "DSResponseDyn".cls
                                                                                typeScalaClass = AnonimousScalaClass
                                                                            }
                                                                            res addMember ("Status = RPCResponseDyn.statusSuccess")
                                                                            res
                                                                        }
                                                                    ),
                                                                    serrializeToOneString = false
                                                                ),
                                                                "resp"
                                                            )
                                                        ),
                                                        ScalaCaseLine(expression = "Failure(_)".expr,
                                                            caseBody = ScalaBody("new DSResponseFailureExDyn(delete)")
                                                        )
                                                    )
                                                )
                                            ),
                                            newLine,
                                            "makeResponce(transaction, sessionContext, recRequestRemoveData, response, requestData)"
                                        )
                                }
                                ScalaMethod(name = suitTraitMethodName, parametrs = ScalaClassParametrs(("request", ScalaHttpServletRequest).param, ("response", ScalaHttpServletResponse).param), parametrsImplicit = ScalaClassParametrs(ScalaClassParametr(name = "system", `type` = "ActorSystem".tp, parametrType = ParametrImplicit)), body = _body)
                            }

                            suitTraitMethods += newLine
                            //</editor-fold>
                        }

                        //<editor-fold desc="Метод onSystemInit">
                        val onSystemInitMethod = ScalaMethod(name = "onSystemInit", `override` = OverrideMofificator, parametrs = ScalaClassParametrs(("system", ScalaActorSystem).param, ("endpoints", ScalaEndpointsAgentPartial).param), body = onSystemInitMethodBody)
                        classServlet addMember newLine
                        classServlet addMember onSystemInitMethod
                        //</editor-fold>

                        //<editor-fold desc="Метод providersPartial">
                        val providersPartionalMethod = ScalaPartialMethod(name = "providersPartial", `type` = ScalaImplicitType, body = providersPartionalMethodBody)
                        classServlet addMember newLine
                        classServlet addMember providersPartionalMethod
                        //</editor-fold>

                        //<editor-fold desc="Трэйт типа trait Suite???? extends CommonSuite with Logging with Config">
                        val suitTrait = new ScalaClassDeclare {
                            typeScalaClass = TypeScalaTrait
                            extensibleClass = ScalaClassCommonSuite
                            withTraits = ScalaWithTraits(ScalaTraitLogging, ScalaTraitConfig)
                            scalaClassGen = ("Suit_" + fullName).cls
                        }
                        //suitTrait addMember ScalaVariable(name = "system", `implicit` = true, serrializeToOneString = true, body = "ActorSystem(getString(\"akka.http.system-name\"))".body)
                        suitTrait addMember newLine
                        suitTrait addMembers (suitTraitMethods: _*)
                        //</editor-fold>

                        classServletes += ScalaComment("Actors")
                        classServletes += newLine

                        actorClasses foreach {
                            clazz =>
                                classServletes += clazz
                                classServletes += newLine
                        }
                        classServletes += ScalaEndComment("Actors")
                        classServletes += newLine
                        classServletes += suitTrait
                        classServletes += newLine
                        classServletes += classServlet

                        //logger trace module.serrialize()

                        val module = ScalaModule(
                            packageName.pkg,
                            newLine,
                            "com.simplesys.jdbc.control.clob._".imp,
                            "com.simplesys.servlet.http.{HttpServletResponse, HttpServletRequest}".imp,
                            "com.simplesys.isc.dataBinging.{DSResponseFailureExDyn, DSResponseFailureDyn, DSRequestDyn, DSResponseDyn}".imp,
                            "com.simplesys.isc.dataBinging.RPC.RPCResponseDyn".imp,
                            "com.simplesys.isc.dataBinging.dataSource.RecordDyn".imp,
                            "com.simplesys.app.SessionContext".imp,
                            "javax.servlet.annotation.WebServlet".imp,
                            "com.simplesys.isc.templates.logJSActor".imp,
                            "com.simplesys.script.JsBeautifier".imp,
                            "com.simplesys.akka.http._".imp,
                            "akka.actor.{Props, ActorRef, Actor, ActorSystem}".imp,
                            "com.simplesys.app.seq.Sequences".imp,
                            "com.simplesys.config.Config".imp,
                            "com.simplesys.log.Logging".imp,
                            "com.simplesys.isc.system.misc._".imp,
                            "com.simplesys.akka.http.Async.Complete".imp,
                            "com.simplesys.isc.grids.RecordsDynList".imp,
                            "org.joda.time.{LocalDateTime, DateTime}".imp,
                            "collection.JavaConversions._".imp,
                            "scalaz.{Failure, Success}".imp,
                            "com.simplesys.jdbc.control.classBO.Where".imp,
                            "com.simplesys.app.servlet.CommonSuite".imp,
                            "com.simplesys.jdbc._".imp,
                            "com.simplesys.json._".imp,
                            "com.simplesys.jdbc.control.DSRequest".imp,
                            "com.simplesys.SQL._".imp,
                            "com.simplesys.tuple._".imp,
                            "com.simplesys.common.Strings._".imp,
                            "com.simplesys.common._".imp
                        )

                        module ++= addedImports
                        module += newLine

                        module ++= (classServletes: _*)

                        //logger trace(newLine + module.serrialize())

                        if (!schema.enumClasses.exists(_.className === boName)) {
                            res <== {
                                out =>
                                    out(genMessageCreating(s"GenBOServlet, stage: $stage"))
                                    out(newLine)
                                    out(newLine)
                                    out(org.scalafmt.Scalafmt.format(module.serrialize()).get)
                            }

                            resSeq += res
                        }
                    }
                }
        }
        resSeq
    }
}
