package ru.simplesys.plugins.sourcegen.app.Gen

import java.net.URI

import com.simplesys.common.Strings._
import com.simplesys.common._
import com.simplesys.common.equality.SimpleEquality._
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
import scalax.file.{Path, PathSet}

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
                                        withTraits = ScalaWithTraits("ServletActorDyn".trt)
                                    }


                                    traitDeclate addMembers(
                                      newLine,
                                      ScalaComment("!!!!!!!!!!!!!!!!!!!!!!!!!!!! DON'T MOVE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"),
                                      ScalaVariable(name = "requestData", body = ScalaBody("new DSRequestDyn(request)"), serrializeToOneString = true),
                                      newLine,
                                      "logger debug s\"Request for " + mode + ": ${newLine + requestData.toPrettyString}\"",
                                      newLine,
                                      ScalaVariable(name = "dataSet", body = ScalaBody( s"""${boName.capitalize}DS(ds)"""), serrializeToOneString = true),
                                      ScalaComment("!!!!!!!!!!!!!!!!!!!!!!!!!! END DON'T MOVE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"),
                                      newLine,
                                      ScalaMethod(name = "receiveBase", `type` = "Option[Actor.Receive]".tp, body = ScalaBody("None"), serrializeToOneString = true)
                                      )

                                    val module = ScalaModule(
                                        packageName.pkg,
                                        newLine,
                                        "com.simplesys.app.SessionContextSupport".imp,
                                        "com.simplesys.isc.system.ServletActorDyn".imp,
                                        "com.simplesys.isc.dataBinging.DSRequestDyn".imp,
                                        "com.simplesys.common.Strings._".imp,
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

                                if (!forLob) {
                                    val fks = clazz.fks.toArray

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

                            def recordDyn(itemName: String, boName: String = strEmpty) = ScalaApplyObject(name = "RecordDyn",
                                parametrs =
                                  ScalaClassParametrs(
                                      (_dataSource \ "Fields" \ "DataSourceFieldDyn") map {
                                          x =>
                                              val name = (x: IscElem).getStringValue("Name")
                                              val jObjectFieldName = (x: IscElem).getStringValue("JObjectFieldName")
                                              val lookup = (x: IscElem).getBooleanValue("Lookup")
                                              val getterType = (x: IscElem).getStringValue("GetterType")
                                              val _boName = if (!forLob) jObjectFieldName.substring(jObjectFieldName.indexOf(".") + 1) + jObjectFieldName.substring(0, jObjectFieldName.indexOf(".")).capitalize else jObjectFieldName.substring(jObjectFieldName.indexOf(".") + 1) + fullClassName.capitalize

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

                            val GetDataBody = ScalaBody()

                            mode match {
                                case "Add" =>
                                    addedImports += s"${pkgBOName}.${groupName}.${boName.capitalize}".imp

                                    GetDataBody ++=(
                                      ScalaVariable(name = "data", serrializeToOneString = true, body = s"requestData.Data".body),
                                      "logger debug s\"data: ${newLine + data.toPrettyString}\"",
                                      newLine,
                                      ScalaVariable(name = s"${boName}Data".unCapitalize, body = ScalaBody(boData(true))),
                                      newLine,
                                      ScalaVariable(name = "insert", body = s"dataSet.insertP(${boName.unCapitalize}Data)".body, serrializeToOneString = true),
                                      newLine,
                                      ScalaApplyObject(name = "Out", parametrs = ScalaClassParametrs(ScalaClassParametr(
                                          name = "classDyn",
                                          `type` = ScalaImplicitType,
                                          defaultValue = ScalaCase(expression = "insert result".expr,
                                              ScalaCaseLine(expression = "Success(res)".expr,
                                                  caseBody = ScalaBody(
                                                      "res foreach (x => logger debug (s\"Inserted: ${x} line(s).\"))",
                                                      newLine,
                                                      ScalaVariable(name = "resp",
                                                          body = ScalaBody(
                                                              {
                                                                  val res = new ScalaClassDeclare {
                                                                      scalaClassGen = "DSResponseDyn".cls
                                                                      typeScalaClass = AnonimousScalaClass
                                                                  }
                                                                  res addMember ("Status = RPCResponseDyn.statusSuccess")

                                                                  res addMember (ScalaVariable(name = "Data", body = ScalaBody(recordDyn(s"${boName}Data".unCapitalize)), variableType = AssignVariable))

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
                                      ))),
                                      newLine,
                                      "selfStop()"
                                      )

                                case "Fetch" =>
                                    addedImports += s"${pkgBOName}.${groupName}.${boName.capitalize}DS".imp

                                    GetDataBody ++=(
                                      ScalaVariable(name = "data", serrializeToOneString = true, body = s"requestData.Data".body),
                                      "logger debug s\"data: ${newLine + data.toPrettyString}\"",
                                      newLine,
                                      ScalaVariable(name = "_data", body = "RecordsDynList()".body, serrializeToOneString = true),
                                      ScalaVariable(name = "qty", `type` = ScalaInt, body = "requestData.EndRow.toInt - requestData.StartRow.toInt".body, serrializeToOneString = true),
                                      newLine,
                                      ScalaVariable(name = "select", body = ScalaBody(s"dataSet.Fetch(dsRequest = DSRequest(sqlDialect = sessionContext.getSQLDialect, startRow = requestData.StartRow, endRow = requestData.EndRow, sortBy = requestData.SortBy, data = data, textMatchStyle = requestData.TextMatchStyle.toString))"), serrializeToOneString = true),
                                      newLine,
                                      ScalaApplyObject(name = "Out", parametrs = ScalaClassParametrs(ScalaClassParametr(
                                          name = "classDyn",
                                          `type` = ScalaImplicitType,
                                          defaultValue = ScalaCase(expression = "select.result".expr,
                                              ScalaCaseLine(expression = "Success(list)".expr,
                                                  caseBody = ScalaBody(
                                                      ScalaControlStruct(name = "list foreach",
                                                          body =
                                                            ScalaBody(
                                                                ScalaCase(
                                                                    ScalaCaseLine(expression = ScalaExpressionBody(ScalaBody(tuple), serrializeToOneString = true),
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
                                                      "logger debug s\"_data: ${newLine + _data.toPrettyString}\"",
                                                      newLine, {
                                                          val res = new ScalaClassDeclare {
                                                              scalaClassGen = "DSResponseDyn".cls
                                                              typeScalaClass = AnonimousScalaClass
                                                          }
                                                          res addMembers(
                                                            "Status = RPCResponseDyn.statusSuccess",
                                                            "Data = _data",
                                                            s"TotalRows = requestData.StartRow.toInt + (if (qty == list.length) qty + (4 * (requestData.EndRow.toInt - requestData.StartRow.toInt)) else list.length)"
                                                            )

                                                          res
                                                      })
                                              ),
                                              ScalaCaseLine(expression = "Failure(_)".expr,
                                                  caseBody = ScalaBody("new DSResponseFailureExDyn(select)")
                                              )
                                          )
                                      ))),
                                      newLine,
                                      "selfStop()"
                                      )

                                case "Update" =>
                                    def getUpdateSatement: ScalaVariable = {
                                        var step = 0
                                        var body = ""

                                        val clazz = schema.resolveClass(LinkRefToClassOld(groupName = groupName, objectName = fullClassName))

                                        clazz.pk.attrNames.foreach {
                                            attr =>
                                                if (step == 0)
                                                    body = s"dataSet.updateP(${boName.unCapitalize}Data, where = Where(dataSet.${attr}${fullClassName.capitalize} === ${boName.unCapitalize}Data.${attr})"
                                                else
                                                    body += space + s"And (dataSet.${attr}${boName.capitalize} === ${boName.unCapitalize}Data.${attr})"
                                                step += 1
                                        }

                                        ScalaVariable(name = "update", body = (body + ")").body, serrializeToOneString = true)
                                    }

                                    GetDataBody ++=(
                                      ScalaVariable(name = "data", body = "requestData.OldValues ++ requestData.Data".body, serrializeToOneString = true),
                                      "logger debug s\"data: ${newLine + data.toPrettyString}\"",
                                      newLine,
                                      ScalaVariable(name = s"${boName.unCapitalize}Data", body = ScalaBody(boData(false)), serrializeToOneString = false),
                                      newLine,
                                      getUpdateSatement,
                                      newLine,
                                      ScalaApplyObject(name = "Out", parametrs = ScalaClassParametrs(ScalaClassParametr(
                                          name = "classDyn",
                                          `type` = ScalaImplicitType,
                                          defaultValue = ScalaCase(expression = "update result".expr,
                                              ScalaCaseLine(expression = "Success(res)".expr,
                                                  caseBody = ScalaBody(
                                                      "res foreach (x => logger debug (s\"Updated: ${x} line(s).\"))",
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
                                        var step = 0
                                        var body = ""

                                        val clazz = schema.resolveClass(LinkRefToClassOld(groupName = groupName, objectName = fullClassName))

                                        clazz.pk.attrNames.foreach {
                                            attr =>
                                                if (step == 0)
                                                    body = s"dataSet.delete(where = Where(dataSet.${attr}${fullClassName.capitalize} === ${attr})"
                                                else
                                                    body += space + s"And (dataSet.${attr}${boName.capitalize} === ${attr})"
                                                step += 1
                                        }

                                        ScalaVariable(name = "delete", body = (body + ")").body, serrializeToOneString = true)
                                    }

                                    GetDataBody ++=(
                                      ScalaVariable(name = "data", body = "requestData.Data".body, serrializeToOneString = true),
                                      "logger debug (s\"data: ${newLine + data.toPrettyString}\")",
                                      newLine,
                                      getPrimarykeyVariables,
                                      getDeleteSatement,
                                      newLine,
                                      ScalaApplyObject(name = "Out", parametrs = ScalaClassParametrs(ScalaClassParametr(
                                          name = "classDyn",
                                          `type` = ScalaImplicitType,
                                          defaultValue = ScalaCase(expression = "delete result".expr,
                                              ScalaCaseLine(expression = "Success(res)".expr,
                                                  caseBody = ScalaBody(
                                                      "res foreach (x => logger debug (s\"Deleted: ${x} line(s).\"))",
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
                                      ))),
                                      newLine,
                                      "selfStop()"
                                      )
                            }

                            val recieveBody = ScalaCase(
                                ScalaCaseLine(
                                    expression = "GetData".expr,
                                    caseBody = GetDataBody),
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
                            packageName.pkg,
                            newLine,
                            "com.simplesys.annotation.RSTransfer".imp,
                            "com.simplesys.app.SessionContextSupport".imp,
                            "com.simplesys.jdbc.control.clob.{Blob, Clob}".imp,
                            "org.joda.time.{LocalDateTime, DateTime}".imp,
                            "com.simplesys.app.seq.Sequences".imp,
                            "com.simplesys.common.Strings._".imp,
                            "com.simplesys.isc.dataBinging.RPC.RPCResponseDyn".imp,
                            "com.simplesys.isc.dataBinging.dataSource.RecordDyn".imp,
                            "com.simplesys.isc.dataBinging.{DSRequestDyn, DSResponseDyn, DSResponseFailureExDyn}".imp,
                            "com.simplesys.isc.grids.RecordsDynList".imp,
                            "com.simplesys.isc.system.{ClassDyn, ServletActorDyn}".imp,
                            "com.simplesys.jdbc._".imp,
                            "com.simplesys.common._".imp,
                            "com.simplesys.jdbc.control.DSRequest".imp,
                            "com.simplesys.jdbc.control.classBO.Where".imp,
                            "com.simplesys.servlet.http.{HttpServletResponse, HttpServletRequest}".imp,
                            "com.simplesys.servlet.{GetData, ServletContext}".imp,
                            "com.simplesys.tuple._".imp,
                            "scalaz.{Failure, Success}".imp
                        )

                        module ++= addedImports
                        module += mainObject

                        module ++= (classServletes: _*)

                        logger debug (newLine + module.serrialize())

                        res <== {
                            out =>
                                out(genMessageCreating(s"GenBOContainer, stage: $stage"))
                                out(newLine)
                                out(newLine)
                                out(module.serrialize())
                        }

                        resSeq += res
                    }

                }
        }

        resSeq
    }

}
