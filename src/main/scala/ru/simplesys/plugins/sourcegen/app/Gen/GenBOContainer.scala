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

class GenBOContainer(val appFilePath: Path,
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
                    val classServletes = ArrayBuffer.empty[ScalaObjectElement]
                    val addedImports = ScalaImports()

                    val dataSource: IscElem = _dataSource
                    val dataSourceIdentifier = (dataSource \ "Identifier").text.replace("_DS", "")
                    val fullClassName = (dataSource \ "fullClassName").text
                    val lobName = (dataSource \ "lobName").text

                    if (dataSourceIdentifier.isEmpty)
                        throw new RuntimeException("Не определен Identifier для DataSource.")

                    if (!servletes.exists(_ === dataSourceIdentifier)) {
                        servletes += dataSourceIdentifier
                        val boName = dataSourceIdentifier.substring(dataSourceIdentifier.indexOf("_") + 1)
                        val groupName = dataSourceIdentifier.substring(0, dataSourceIdentifier.indexOf("_"))

                        val forLob = !(boName == fullClassName)

                        val firstAddImport = s"${pkgBOName}.${groupName}._".imp
                        addedImports += firstAddImport

                        val fullName = s"${groupName}_${boName}"
                        val res = (outFilePath / "scala" / "container" / s"${fullName}_Container.scala").createFile(failIfExists = false).toFile

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

                        val mainObject = new ScalaClassDeclare {
                            scalaClassGen = (s"${boName.capitalize}Container").cls
                            typeScalaClass = TypeScalaObject
                        }

                        val recordTrait = new ScalaClassDeclare {
                            scalaClassGen = (s"${boName.capitalize}DataRecord").cls
                            typeScalaClass = TypeScalaTrait
                            //annotation = ScalaAnnotation("ScalaJSDefined")
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

                        for (mode <- operationTypes; _dataURL <- (dataSource \ (mode + "DataURL"))) {
                            val url = s"logic/$fullName/$mode"
                            val urlVar = ScalaVariable(name = s"${fullName}_$mode", serrializeToOneString = true, body = url.dblQuoted.body)
                            val actorAnnotation = ScalaAnnotation("RSTransfer", "urlPattern" -> s"/$url".dblQuoted)

                            def makeSemiHandMakeTrait(mode: String): String = {
                                val fileTraitPath = sourceMain / "scala" / "com" / "simplesys" / "container" / "SemiHandTraits" / (s"${fullName}_SemiHandTrait_$mode.scala")
                                val traitName = s"${fullName}_SemiHandTrait_$mode"

                                if (!fileTraitPath.exists) {
                                    val fileTrait = fileTraitPath.createFile(failIfExists = false).toFile

                                    val traitDeclate = new ScalaClassDeclare {
                                        scalaClassGen = traitName.cls
                                        typeScalaClass = TypeScalaTrait
                                        extensibleClass = "SessionContextSupport".ext
                                        withTraits = ScalaWithTraits("ServletActor".trt)
                                    }


                                    traitDeclate addMembers(
                                      newLine,
                                      ScalaComment("!!!!!!!!!!!!!!!!!!!!!!!!!!!! DON'T MOVE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"),
                                      ScalaVariable(name = "requestData", body = ScalaBody("new DSRequest(request.JSONData)"), serrializeToOneString = true),
                                      newLine,
                                      "logger debug s\"Request for " + mode + ": ${newLine + requestData.toPrettyString}\"",
                                      newLine,
                                      ScalaVariable(name = "dataSet", body = ScalaBody( s"""${boName.capitalize}DS(oraclePool)"""), serrializeToOneString = true),
                                      ScalaComment("!!!!!!!!!!!!!!!!!!!!!!!!!! END DON'T MOVE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"),
                                      newLine,
                                      ScalaMethod(name = "receiveBase", `type` = "Option[Actor.Receive]".tp, body = ScalaBody("None"), serrializeToOneString = true),
                                      newLine,
                                      ScalaMethod(
                                          name = "wrapperBlobGetter",
                                          `type` = ScalaString,
                                          parametrs = ScalaClassParametrs(
                                              ScalaClassParametr(
                                                  name = "blob",
                                                  `type` = "Blob".tp
                                              )
                                          ),
                                          body = ScalaBody("blob.asString"),
                                          serrializeToOneString = true)
                                    )

                                    val module = ScalaModule(
                                        s"$packageName.$groupName".pkg,
                                        newLine,
                                        "com.simplesys.app.SessionContextSupport".imp,
                                        "com.simplesys.servlet.isc.ServletActor".imp,
                                        "com.simplesys.common.Strings._".imp,
                                        "com.simplesys.jdbc.control.clob._".imp,
                                        "com.simplesys.isc.dataBinging.DSRequest".imp,
                                        "akka.actor.Actor".imp,
                                        firstAddImport,
                                        newLine,
                                        traitDeclate
                                    )

                                    fileTrait <== {
                                        out =>
                                            out(genMessageCreating(s"GenBOContainer, stage: $stage"))
                                            out(newLine)
                                            out(newLine)
                                            out(module.serrialize())
                                    }

                                    resSeq += fileTrait
                                }

                                traitName
                            }

                            val actorClass = new ScalaClassDeclare {
                                annotation = actorAnnotation
                                extensibleClass = makeSemiHandMakeTrait(mode).ext
                                scalaClassGen = s"${mode}Actor".cls
                                parametrs = ScalaClassParametrs(
                                    ScalaClassParametr(name = "request", `type` = ScalaHttpServletRequest, parametrType = ParametrVal),
                                    ScalaClassParametr(name = "response", `type` = ScalaHttpServletResponse, parametrType = ParametrVal),
                                    ScalaClassParametr(name = "servletContext", `type` = "ServletContext".tp, parametrType = ParametrVal)
                                )
                            }

                            def tuple: ScalaApplyObject = {
                                var i = 0
                                val allColumns = ArrayBuffer.empty[String]

                                val clazz = schema.resolveClass(LinkRefToClassOld(groupName = groupName, objectName = fullClassName))

                                val attrs: Array[AttrDef[_]] = if (!forLob) clazz.attrsWithOutLob.toArray
                                else {
                                    val attrPk: Array[AttrDef[_]] = clazz.strictUCs.filter(_.ucType == PK).flatMap(_.attrNames).map(clazz.attr).toArray
                                    val attr = clazz.attr(lobName)

                                    attrPk ++ Array(attr)
                                }

                                Sorting.quickSort(attrs)(AttrDefOrd)

                                def getParams(_fkName: String, attrs: Seq[AttrDef[_]], clazz: IClass): Seq[ScalaClassParametr] = {
                                    val res = ArrayBuffer.empty[ScalaClassParametr]
                                    attrs foreach {
                                        attr =>
                                            val fieldName = s"${attr.name}${clazz.className.capitalize}" + {
                                                if (_fkName.isEmpty) strEmpty else s"_${_fkName}"
                                            }

                                            val _columnType = {
                                                val res = attr.attrType.scalaTypeAsString(clazz.group, schema)
                                                if (res.indexOf(".") !== -1) {
                                                    addedImports += res.imp
                                                    logger debug (s"Bad type: ${res.dblQuoted} transform to ${res.substring(res.lastIndexOf(".") + 1).dblQuoted} and added import ${res}")

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
                                params ++= (getParams(strEmpty, attrs, clazz): _*)

                                if (!forLob) {
                                    val fks = clazz.fks.toArray

                                    Sorting.quickSort(fks)(ForeignKeyConstraintDefOrd)
                                    fks foreach {
                                        case fk: ForeignKeyConstraintDef =>
                                            val fkClass = fk.referencedClassRef.toClass
                                            if (fkClass.className != clazz.className || fkClass.group != clazz.group) {

                                                val attrs1: Array[AttrDef[_]] = fkClass.pk.attrs.toArray
                                                Sorting.quickSort(attrs1)(AttrDefOrd)
                                                params ++= (getParams(fk.softNameForCompare, attrs1, fkClass): _*)

                                                val attrs2: Array[AttrDef[_]] = fkClass.defaultSettings.showAttrsResult.toArray
                                                Sorting.quickSort(attrs2)(AttrDefOrd)
                                                params ++= (getParams(fk.softNameForCompare, attrs2, fkClass): _*)
                                            }
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
                                                                parametrs += ScalaClassParametr(name = name, `type` = ScalaImplicitType, defaultValue = "Sequences(oraclePool).nextLong1(dataSet.fromBO.fromTable.databaseTablename)")
                                                            case "LongOpt" =>
                                                                parametrs += ScalaClassParametr(name = name, `type` = ScalaImplicitType, defaultValue = "Some(Sequences(oraclePool).nextLong1(dataSet.fromBO.fromTable.databaseTablename))")
                                                            case "Double" =>
                                                                parametrs += ScalaClassParametr(name = name, `type` = ScalaImplicitType, defaultValue = "Sequences(oraclePool).nextDouble1(dataSet.fromBO.fromTable.databaseTablename)")
                                                            case "BigDecimal" =>
                                                                parametrs += ScalaClassParametr(name = name, `type` = ScalaImplicitType, defaultValue = "Sequences(oraclePool).nextBigDecimal1(dataSet.fromBO.fromTable.databaseTablename)")
                                                            case "String" =>
                                                                parametrs += ScalaClassParametr(name = name, `type` = ScalaImplicitType, defaultValue = "getGUID")
                                                            case x =>
                                                                throw new RuntimeException(s"GenBySeq must be BigDecimal or Long or String or Double, but exist (${x})")
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

                            def recordDyn(itemName: String, boName: String = strEmpty) = ScalaApplyObject(
                                name = "fromJsonObject(JsonObject.fromIterable(Seq",
                                parametrs = ScalaClassParametrs(
                                    (_dataSource \ "Fields" \ "DataSourceFieldDyn") map {
                                        x =>
                                            val name = (x: IscElem).getStringValue("Name")
                                            val foreignKey = (x: IscElem).getStringValue("ForeignField")
                                            val jObjectFieldName = (x: IscElem).getStringValue("JObjectFieldName")
                                            val lookup = (x: IscElem).getBooleanValue("Lookup")
                                            val getterType: String = (x: IscElem).getStringValue("GetterType")
                                            val _boName = if (!forLob) jObjectFieldName.substring(jObjectFieldName.indexOf(".") + 1) + jObjectFieldName.substring(0, jObjectFieldName.indexOf(".")).capitalize else jObjectFieldName.substring(jObjectFieldName.indexOf(".") + 1) + fullClassName.capitalize

                                            def blobWrapper(str: String): String = {
                                                if (getterType == "Blob")
                                                    s"wrapperBlobGetter($str)"
                                                else
                                                    str
                                            }

                                            if (itemName == strEmpty) {
                                                if (!lookup)
                                                    ScalaClassParametr(
                                                        name = name.dblQuoted,
                                                        `type` = ScalaImplicitType,
                                                        defaultValue = blobWrapper(_boName),
                                                        sign = ScalaSignArrowRight
                                                    )
                                                else
                                                    ScalaClassParametr(
                                                        name = s"${name}_${foreignKey.capitalize}".dblQuoted,
                                                        `type` = ScalaImplicitType,
                                                        defaultValue = blobWrapper(s"${_boName}_${foreignKey.capitalize}"),
                                                        sign = ScalaSignArrowRight
                                                    )
                                            }
                                            else if (!lookup)
                                                ScalaClassParametr(
                                                    name = name.dblQuoted,
                                                    `type` = ScalaImplicitType,
                                                    defaultValue = blobWrapper({
                                                        if (itemName.isEmpty) s"${name}" else s"${itemName}.${name}"
                                                    } + boName), sign = ScalaSignArrowRight)
                                            else
                                                ScalaClassParametr(
                                                    name = s"${name}_${foreignKey.capitalize}".dblQuoted,
                                                    `type` = ScalaImplicitType,
                                                    defaultValue = blobWrapper(s"data.get${getterType}(${s"${name}_${foreignKey.capitalize}".dblQuoted})"),
                                                    sign = ScalaSignArrowRight
                                                )
                                    }: _*
                                ),
                                suffix = "))"
                            )

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

                            val getDataBody = ScalaBody()

                            def getDSResponse(body: ScalaBody): ScalaBody = {
                                val res = ScalaApplyObject(
                                    name = "DSResponse",
                                    parametrs = ScalaClassParametrs(
                                        ScalaClassParametr(
                                            name = "status",
                                            `type` = ScalaImplicitType,
                                            defaultValue = "RPCResponse.statusSuccess"
                                        ),
                                        ScalaClassParametr(
                                            name = "data",
                                            `type` = ScalaImplicitType,
                                            defaultValue = body
                                        )
                                    ),
                                    suffix = ".asJson"
                                )

                                ScalaBody(res)
                            }

                            mode match {
                                case "Add" =>
                                    addedImports += s"${pkgBOName}.${groupName}.${boName.capitalize}".imp

                                    val insertBody = ScalaCase(
                                        expression = "requestData.transaction.getOrElse(Transaction()).transactionNum".expr,
                                        ScalaCaseLine(
                                            expression = "None".expr,
                                            caseBody = ScalaBody(
                                                ScalaVariable(
                                                    name = "data",
                                                    body = "requestData.data".body,
                                                    serrializeToOneString = true
                                                ),
                                                newLine,
                                                "logger debug s\"data: ${newLine + data.toPrettyString}\"",
                                                newLine,
                                                ScalaVariable(name = s"${boName}Data".unCapitalize, body = ScalaBody(boData(true))),
                                                newLine,
                                                ScalaVariable(
                                                    name = "listResponse",
                                                    variableType = AssignVariable,
                                                    sign = "append",
                                                    body = getDSResponse(ScalaBody(recordDyn(s"${boName}Data".unCapitalize))),
                                                    serrializeToOneString = true
                                                ),
                                                newLine,
                                                s"dataSet.insertP(${boName.unCapitalize}Data)"
                                            )
                                        ),
                                        ScalaCaseLine(
                                            expression = "Some(transactionNum)".expr,
                                            caseBody = ScalaBody(
                                                ScalaControlStruct(
                                                    name = "transaction(dataSet.dataSource)",
                                                    body = ScalaControlBody(
                                                        expression = "connection".expr,
                                                        ScalaExpression("_transactionNum = Some(transactionNum)"),
                                                        newLine,
                                                        ScalaVariable(name = "values",
                                                            `type` = s"Seq[${boName.capitalize}]".tp,
                                                            body = ScalaBody(
                                                                ScalaControlStruct(
                                                                    name = "requestData.transaction.getOrElse(Transaction()).operations.map",
                                                                    body = ScalaControlBody(
                                                                        expression = "operation".expr,
                                                                        ScalaVariable(
                                                                            name = "data",
                                                                            body = "operation.getJsonObject(\"data\")".body,
                                                                            serrializeToOneString = true
                                                                        ),
                                                                        newLine,
                                                                        "logger debug (s\"data: ${newLine + data.toPrettyString}\")",
                                                                        newLine,
                                                                        ScalaVariable(
                                                                            name = s"${boName}Data".unCapitalize,
                                                                            body = ScalaBody(boData(true))),
                                                                        newLine,
                                                                        ScalaVariable(
                                                                            name = "listResponse",
                                                                            variableType = AssignVariable,
                                                                            sign = "append",
                                                                            body = getDSResponse(ScalaBody(recordDyn(s"${boName}Data".unCapitalize))),
                                                                            serrializeToOneString = true
                                                                        ),
                                                                        newLine,
                                                                        ScalaExpression("SendMessage(Message(channels = s\"ListElements_Add_$transactionNum\"))"),
                                                                        newLine,
                                                                        s"${boName.unCapitalize}Data"
                                                                    )
                                                                ),
                                                                "dataSet.insertPWithoutCommit(connection = connection, values: _*)"
                                                            )
                                                        )
                                                    ))
                                            )
                                        ))

                                    getDataBody ++= (
                                      "import com.simplesys.messages.ActorConfig._",
                                      newLine,
                                      ScalaVariable(name = "listResponse", serrializeToOneString = true, body = s"ArrayBuffer.empty[Json]".body),
                                      newLine,
                                      ScalaVariable(name = "_transactionNum", `type` = "Option[String]".tp, variableType = VariableVar, serrializeToOneString = true, body = s"None".body),
                                      newLine,
                                      "logger debug s\"request: ${newLine + requestData.toPrettyString}\"",
                                      newLine,
                                      ScalaVariable(
                                          name = "insert",
                                          `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("Array", ScalaGenerics("Int")))),
                                          body = ScalaBody(insertBody),
                                          serrializeToOneString = true
                                      ),
                                      newLine,
                                      ScalaApplyObject(name = "Out", parametrs = ScalaClassParametrs(
                                          ScalaClassParametr(
                                              name = "out",
                                              `type` = ScalaImplicitType,
                                              defaultValue = ScalaCase(expression = "insert result".expr,
                                                  ScalaCaseLine(expression = "Success(res)".expr,
                                                      caseBody = ScalaBody(
                                                          "res foreach (x => logger debug (s\"Inserted: ${x} line(s).\"))",
                                                          "arr(listResponse: _*)".body
                                                      )
                                                  ),
                                                  ScalaCaseLine(expression = "Failure(_)".expr,
                                                      caseBody = ScalaBody("DSResponseFailureEx(insert.printException.get.message, insert.printException.get.stackTrace).asJson")
                                                  )
                                              )
                                          ))),
                                      newLine,
                                      ScalaIf(ScalaExpression("_transactionNum.isDefined"), ScalaBody("SendMessage(Message(channels = s\"ListElements_EndAdd_${_transactionNum.get}\"))"), serrializeToOneString = true),
                                      newLine,
                                      "selfStop()"
                                    )

                                case "Fetch" =>
                                    addedImports += s"${pkgBOName}.${groupName}.${boName.capitalize}DS".imp

                                    getDataBody ++= (
                                      "logger debug s\"request: ${newLine + requestData.toPrettyString}\"",
                                      newLine,
                                      ScalaVariable(name = "data", serrializeToOneString = true, body = s"requestData.data".body),
                                      "logger debug s\"data: ${newLine + data.toPrettyString}\"",
                                      newLine,
                                      ScalaVariable(name = "_data", body = "ArrayBuffer.empty[Json]".body, serrializeToOneString = true),
                                      ScalaVariable(name = "qty", `type` = ScalaInt, body = "requestData.endRow.getOrElse(1) - requestData.startRow.getOrElse(0) + 1".body, serrializeToOneString = true),
                                      newLine,
                                      ScalaVariable(
                                          name = "select",
                                          body = ScalaBody(
                                              s"dataSet.Fetch(dsRequest = DsRequest(sqlDialect = sessionContext.getSQLDialect, startRow = requestData.startRow.getOrElse(0), endRow = requestData.endRow.getOrElse(0), sortBy = requestData.sortBy.getOrElse(Json.Null), data = data, textMatchStyle = requestData.textMatchStyle.toString))"),
                                          serrializeToOneString = true),
                                      newLine,
                                      ScalaApplyObject(name = "Out", parametrs = ScalaClassParametrs(
                                          ScalaClassParametr(
                                              name = "out",
                                              `type` = ScalaImplicitType,
                                              defaultValue = ScalaCase(expression = "select.result".expr,
                                                  ScalaCaseLine(expression = "Success(list)".expr,
                                                      caseBody = ScalaBody(
                                                          ScalaControlStruct(name = "list foreach",
                                                              body =
                                                                ScalaBody(
                                                                    ScalaCase(
                                                                        ScalaCaseLine(
                                                                            expression = ScalaExpressionBody(ScalaBody(tuple),
                                                                                serrializeToOneString = true
                                                                            ),
                                                                            caseBody = ScalaBody(
                                                                                ScalaVariable(name = "_data", variableType = AssignVariable, sign = "+=",
                                                                                    body = ScalaBody(recordDyn(strEmpty, boName)),
                                                                                    serrializeToOneString = true
                                                                                )
                                                                            )
                                                                        ),
                                                                        ScalaCaseLine(
                                                                            expression = "x".expr,
                                                                            caseBody = "new RuntimeException (s\"mached as : $x\")".body
                                                                        )
                                                                    )
                                                                ),
                                                              serrializeToOneString = true
                                                          ),
                                                          newLine,
                                                          "logger debug s\"_data: ${newLine + arr(_data:_*).toPrettyString}\"",
                                                          newLine, {
                                                              val res = ScalaApplyObject(
                                                                  name = "DSResponse",
                                                                  parametrs = ScalaClassParametrs(
                                                                      ScalaClassParametr(
                                                                          name = "status",
                                                                          `type` = ScalaImplicitType,
                                                                          defaultValue = "RPCResponse.statusSuccess"
                                                                      ),
                                                                      ScalaClassParametr(
                                                                          name = "data",
                                                                          `type` = ScalaImplicitType,
                                                                          defaultValue = "arr(_data:_*)"
                                                                      ),
                                                                      ScalaClassParametr(
                                                                          name = "totalRows",
                                                                          `type` = ScalaImplicitType,
                                                                          defaultValue = "Some(requestData.startRow.getOrElse(0) + (if (qty == list.length) qty * 2 else list.length))"
                                                                      )
                                                                  ),
                                                                  suffix = ".asJson"
                                                              )
                                                              res
                                                          })
                                                  ),
                                                  ScalaCaseLine(expression = "Failure(_)".expr,
                                                      caseBody = ScalaBody("DSResponseFailureEx(select.printException.get.message, select.printException.get.stackTrace).asJson")
                                                  )
                                              )
                                          ))),
                                      newLine,
                                      "selfStop()"
                                    )

                                case "Update" =>
                                    def getUpdateSatement: ScalaVariable = {

                                        def getBody(statement: String): String = {
                                            val body = new StringBuilder
                                            val clazz = schema.resolveClass(LinkRefToClassOld(groupName = groupName, objectName = fullClassName))

                                            clazz.pk.attrNames.zipWithIndex.foreach {
                                                case (attr, index) =>
                                                    if (index == 0)
                                                        body append s"${statement} values = ${boName.unCapitalize}Data, where = Where(dataSet.${attr}${fullClassName.capitalize} === ${boName.unCapitalize}Data.${attr})"
                                                    else
                                                        body append space + s"And (dataSet.${attr}${fullClassName.capitalize} === ${boName.unCapitalize}Data.${attr})"

                                            }
                                            body.toString()
                                        }

                                        val updateBody = ScalaCase(
                                            expression = "requestData.transaction.getOrElse(Transaction()).transactionNum".expr,
                                            ScalaCaseLine(
                                                expression = "None".expr,
                                                caseBody = ScalaBody(
                                                    ScalaVariable(
                                                        name = "data",
                                                        body = "requestData.oldValues ++ requestData.data".body,
                                                        serrializeToOneString = true
                                                    ),
                                                    newLine,
                                                    "logger debug s\"data: ${newLine + data.toPrettyString}\"",
                                                    ScalaVariable(name = s"${boName.unCapitalize}Data", body = ScalaBody(boData(false)), serrializeToOneString = false),
                                                    newLine,
                                                    ScalaVariable(
                                                        name = "listResponse",
                                                        variableType = AssignVariable,
                                                        sign = "append",
                                                        body = getDSResponse(ScalaBody(recordDyn(s"${boName}Data".unCapitalize))
                                                        ),
                                                        serrializeToOneString = true),
                                                    newLine,
                                                    getBody("dataSet.updateP(") + ")"
                                                )
                                            ),
                                            ScalaCaseLine(
                                                expression = "_".expr,
                                                caseBody = ScalaBody(
                                                    ScalaControlStruct(
                                                        name = "transaction(dataSet.dataSource)",
                                                        body = ScalaControlBody(
                                                            expression = "connection".expr,
                                                            ScalaControlStruct(
                                                                name = "requestData.transaction.getOrElse(Transaction()).operations.flatMap",
                                                                body = ScalaControlBodyWithSuffix(
                                                                    expression = "operation".expr,
                                                                    suffix = ".toArray",
                                                                    ScalaBody(
                                                                        ScalaVariable(
                                                                            name = "data",
                                                                            body = "operation.getJsonObjectOpt(\"oldValues\") ++ operation.getJsonObjectOpt(\"data\")".body,
                                                                            serrializeToOneString = true
                                                                        ),
                                                                        "logger debug (s\"data: ${newLine + data.toPrettyString}\")",
                                                                        newLine,
                                                                        ScalaVariable(name = s"${boName.unCapitalize}Data", body = ScalaBody(boData(false)), serrializeToOneString = false),
                                                                        newLine,
                                                                        ScalaVariable(
                                                                            name = "listResponse",
                                                                            variableType = AssignVariable,
                                                                            sign = "append",
                                                                            body = getDSResponse(ScalaBody(recordDyn(s"${boName}Data".unCapitalize))),
                                                                            serrializeToOneString = true),
                                                                        getBody("dataSet.updatePWithoutCommit(connection = connection,") + ")"
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )

                                        ScalaVariable(
                                            name = "update",
                                            `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("Array", ScalaGenerics("Int")))),
                                            body = ScalaBody(updateBody), serrializeToOneString = true
                                        )
                                    }

                                    getDataBody ++= (
                                      "logger debug s\"request: ${newLine + requestData.toPrettyString}\"",
                                      newLine,
                                      ScalaVariable(name = "listResponse", serrializeToOneString = true, body = s"ArrayBuffer.empty[Json]".body),
                                      newLine,
                                      getUpdateSatement,
                                      newLine,
                                      ScalaApplyObject(name = "Out", parametrs = ScalaClassParametrs(ScalaClassParametr(
                                          name = "out",
                                          `type` = ScalaImplicitType,
                                          defaultValue = ScalaCase(expression = "update result".expr,
                                              ScalaCaseLine(expression = "Success(res)".expr,
                                                  caseBody = ScalaBody(
                                                      "res foreach (x => logger debug (s\"Updated: ${x} line(s).\"))",
                                                      "arr(listResponse: _*)".body
                                                  )
                                              ),
                                              ScalaCaseLine(expression = "Failure(_)".expr,
                                                  caseBody = ScalaBody("DSResponseFailureEx(update.printException.get.message, update.printException.get.stackTrace).asJson")
                                              )
                                          )
                                      ))),
                                      newLine,
                                      "selfStop()"
                                    )

                                case "Remove" =>
                                    def getPrimarykeyVariables: ScalaVariables = {
                                        ScalaVariables(getPkField.map {
                                            case PkData(getterType: String, pk: String) =>
                                                ScalaVariable(name = pk, body = s"data.get${getterType}(${pk.dblQuoted})".body, serrializeToOneString = true)
                                        }: _*)
                                    }

                                    def getDeleteSatement: ScalaVariable = {
                                        def getBody(statement: String): String = {
                                            val body = new StringBuilder
                                            val clazz = schema.resolveClass(
                                                LinkRefToClassOld(
                                                    groupName = groupName,
                                                    objectName = fullClassName
                                                ))

                                            clazz.pk.attrNames.zipWithIndex.foreach {
                                                case (attr, index) =>
                                                    if (index == 0)
                                                        body append s"$statement.${attr}${fullClassName.capitalize} === ${attr})"
                                                    else
                                                        body append space + s"And (dataSet.${attr}${fullClassName.capitalize} === ${attr})"
                                            }
                                            body.toString()
                                        }

                                        val deleteBody = ScalaCase(
                                            expression = "requestData.transaction.getOrElse(Transaction()).transactionNum".expr,
                                            ScalaCaseLine(
                                                expression = "None".expr,
                                                caseBody = ScalaBody(
                                                    ScalaVariable(
                                                        name = "data",
                                                        body = "requestData.data".body,
                                                        serrializeToOneString = true
                                                    ),
                                                    newLine,
                                                    "logger debug (s\"data: ${newLine + data.toPrettyString}\")",
                                                    getPrimarykeyVariables,
                                                    ScalaVariable(
                                                        name = "listResponse",
                                                        variableType = AssignVariable,
                                                        sign = "append",
                                                        body = getDSResponse(ScalaBody("arr()")),
                                                        serrializeToOneString = true),
                                                    getBody("dataSet.delete(where = Where(dataSet") + ")"
                                                )
                                            ),
                                            ScalaCaseLine(
                                                expression = "Some(transactionNum)".expr,
                                                caseBody = ScalaBody(
                                                    ScalaControlStruct(
                                                        name = "transaction(dataSet.dataSource)",
                                                        body = ScalaControlBody(
                                                            expression = "connection".expr,
                                                            ScalaExpression("_transactionNum = Some(transactionNum)"),
                                                            newLine,
                                                            ScalaControlStruct(
                                                                name = "requestData.transaction.getOrElse(Transaction()).operations.flatMap",
                                                                body = ScalaControlBodyWithSuffix(
                                                                    expression = "operation".expr,
                                                                    suffix = ".toArray",
                                                                    ScalaBody(
                                                                        ScalaVariable(
                                                                            name = "data",
                                                                            body = "operation.getJsonObjectOpt(\"data\")".body,
                                                                            serrializeToOneString = true
                                                                        ),
                                                                        "logger debug (s\"data: ${newLine + data.toPrettyString}\")",
                                                                        newLine,
                                                                        getPrimarykeyVariables,
                                                                        ScalaVariable(
                                                                            name = "listResponse",
                                                                            variableType = AssignVariable,
                                                                            sign = "append",
                                                                            body = getDSResponse(ScalaBody("arr()")),
                                                                            serrializeToOneString = true),
                                                                        ScalaExpression("SendMessage(Message(channels = s\"ListElements_Remove_$transactionNum\"))"),
                                                                        newLine,
                                                                        getBody("dataSet.deleteWithoutCommit(connection = connection, where = Where(dataSet") + ")"
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )

                                        ScalaVariable(
                                            name = "delete",
                                            `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("Array", ScalaGenerics("Int")))),
                                            body = ScalaBody(deleteBody),
                                            serrializeToOneString = true
                                        )
                                    }

                                    getDataBody ++= (
                                      "import com.simplesys.messages.ActorConfig._",
                                      newLine,
                                      ScalaVariable(name = "_transactionNum", `type` = "Option[String]".tp, variableType = VariableVar, serrializeToOneString = true, body = s"None".body),
                                      newLine,
                                      "logger debug s\"request: ${newLine + requestData.toPrettyString}\"",
                                      newLine,
                                      ScalaVariable(name = "listResponse", serrializeToOneString = true, body = s"ArrayBuffer.empty[Json]".body),
                                      getDeleteSatement,
                                      newLine,
                                      ScalaApplyObject(
                                          name = "Out",
                                          parametrs = ScalaClassParametrs(ScalaClassParametr(
                                              name = "out",
                                              `type` = ScalaImplicitType,
                                              defaultValue = ScalaCase(
                                                  expression = "delete result".expr,
                                                  ScalaCaseLine(expression = "Success(res)".expr,
                                                      caseBody = ScalaBody(
                                                          "res foreach (x => logger debug (s\"Deleted: ${x} line(s).\"))",
                                                          "arr(listResponse: _*)".body
                                                      )
                                                  ),
                                                  ScalaCaseLine(expression = "Failure(_)".expr,
                                                      caseBody = ScalaBody("DSResponseFailureEx(delete.printException.get.message, delete.printException.get.stackTrace).asJson")
                                                  )
                                              )
                                          ))),
                                      newLine,
                                      ScalaIf(ScalaExpression("_transactionNum.isDefined"), ScalaBody("SendMessage(Message(channels = s\"ListElements_EndRemove_${_transactionNum.get}\"))"), serrializeToOneString = true),
                                      newLine,
                                      "selfStop()"
                                    )
                            }

                            val recieveBody = ScalaCase(
                                ScalaCaseLine(
                                    expression = "GetData".expr,
                                    caseBody = getDataBody),
                                ScalaCaseLine(
                                    expression = "x".expr,
                                    caseBody = "throw new RuntimeException(s\"Bad branch $x\")".body)
                            )

                            actorClass addMembers (
                              ScalaPartialMethod(
                                  name = "receive",
                                  body = ScalaIf("receiveBase.isEmpty".expr, ScalaBody(recieveBody), ScalaBody("receiveBase.get"))
                              ))

                            mainObject addMembers(newLine, urlVar, newLine, actorClass)
                        }

                        val module = ScalaModule(
                            s"$packageName.$groupName".pkg,
                            newLine,
                            "com.simplesys.annotation.RSTransfer".imp,
                            "com.simplesys.servlet.ServletContext".imp,
                            "com.simplesys.app.SessionContextSupport".imp,
                            "com.simplesys.jdbc.control.clob._".imp,
                            "java.time.LocalDateTime".imp,
                            "com.simplesys.isc.dataBinging.{DSResponse, RPCResponse, Transaction, DSResponseFailureEx}".imp,
                            "com.simplesys.circe.Circe._".imp,
                            "scala.collection.mutable.ArrayBuffer".imp,
                            "com.simplesys.app.seq.Sequences".imp,
                            "com.simplesys.common.Strings._".imp,
                            "com.simplesys.jdbc._".imp,
                            "com.simplesys.common._".imp,
                            "com.simplesys.circe.Circe._".imp,
                            "io.circe.syntax._".imp,
                            "io.circe.generic.auto._".imp,
                            "com.simplesys.jdbc.control.{DsRequest, ValidationEx}".imp,
                            "com.simplesys.jdbc.control.classBO.Where".imp,
                            "com.simplesys.servlet.http.{HttpServletResponse, HttpServletRequest}".imp,
                            "com.simplesys.servlet.isc.GetData".imp,
                            "com.simplesys.jdbc.control.SessionStructures._".imp,
                            "com.simplesys.tuple._".imp,
                            "io.circe.{Json, JsonObject}".imp,
                            "io.circe.Json._".imp,
                            "com.simplesys.servlet.isc.GetData".imp,
                            "scalaz.{Failure, Success}".imp,
                            "com.simplesys.isc.system.misc.Number".imp,
                            "com.simplesys.common.array._".imp,
                            "com.simplesys.messages.Message".imp,
                            "scala.scalajs.js".imp
                        )

                        module ++= addedImports
                        module += recordTrait
                        module += mainObject

                        module ++= (classServletes: _*)

                        logger debug (newLine + module.serrialize())

                        res <== {
                            out =>
                                out(genMessageCreating(s"GenBOContainer, stage: $stage"))
                                out(newLine)
                                out(newLine)
                                out(org.scalafmt.Scalafmt.format(module.serrialize()).get)
                        }

                        resSeq += res
                    }

                }
        }

        resSeq
    }

}
