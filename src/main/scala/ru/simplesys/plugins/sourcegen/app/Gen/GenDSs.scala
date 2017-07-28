package ru.simplesys.plugins.sourcegen.app.Gen

import java.io.File
import java.net.URI

import com.simplesys.common.Strings._
import com.simplesys.common._
import com.simplesys.genSources._
import com.simplesys.io._
import com.simplesys.scalaGen._
import ru.simplesys.plugins.sourcegen.app._
import ru.simplesys.plugins.sourcegen.meta._
import sbt.Logger

import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting
import com.simplesys.file.{Path, PathSet}

class GenDSs(val appFilePath: Path,
             val outFilePath: Path,
             val packageName: String,
             val pkgBOName: String,
             val quoted: Boolean,
             val logger: Logger) extends GenScala1 with Log {

    val schemaPath: URI = "".xsdURI

    val sourceBOFiles: PathSet[Path] = appFilePath * "*.xml"
    implicit val schema = SchemaDef(pkgBOName, sourceBOFiles.files)

    def create: File = ????

    private def genDSs(clazz: IClass): Seq[File] = {
        val res = ArrayBuffer.empty[File]

        val attrs: Array[AttrDef[_]] = clazz.attrsWithOutLob.toArray
        Sorting.quickSort(attrs)(AttrDefOrd)

        val className = clazz.className.ds
        res += genDS(attrs, "", false, clazz)


        clazz.attrsWithLob.toArray.foreach {
            attr =>
                val attrPk = clazz.strictUCs.filter(_.ucType == PK).flatMap(_.attrNames).map(clazz.attr).toArray
                res += genDS(attrPk ++ Array(attr), attr.name, true, clazz)
        }

        res
    }

    private def genDS(attrs: Array[AttrDef[_]], attrName: String, forLob: Boolean, clazz: IClass): File = {

        val className = s"${clazz.className}${attrName.capitalize}".ds
        val productName = s"${clazz.className}${attrName.capitalize}".ds + "Data"

        val addImports = ArrayBuffer.empty[ScalaObjectElement]
        val res: File = (outFilePath / clazz.group / (className + ".scala")).createFile(failIfExists = false).toFile
        logger debug (s"Class: ${className}")

        val dsClass = new ScalaClassDeclare {
            scalaClassGen = className.cls
            parametrs = ScalaClassParametrs(
                ScalaClassParametr(name = "alias", `type` = "SQLAlias".tp, parametrType = ParametrVal, `override` = OverrideMofificator)
            )
            parametrsImplicit = ScalaClassParametrs(
                ScalaClassParametr(name = "dataSource", `type` = ScalaBoneCPDataSource, parametrType = ParametrImplicitVal)
            )
            extensibleClass = ScalaClassGenericExtensible(new ScalaBaseClassDeclare {
                scalaClassGen = "DataSet".cls
                generics = ScalaGeneric(className)
            })
        }

        var j = 0
        val allColumns = ArrayBuffer.empty[String]
        var tupleType = strEmpty
        val dsCaseClassParameters = ScalaClassParametrs()

        def selectPMethod(name: String, returnType: AbstractScalaType, sb: => ScalaBody) =
            ScalaMethod(
                name = name,
                parametrs = ScalaClassParametrs(
                    ScalaClassParametr(name = "join", `type` = "JoinParam".tp, defaultValue = "null"),
                    ScalaClassParametr(name = "where", `type` = "WhereParam".tp, defaultValue = "null"),
                    ScalaClassParametr(name = "orderBy", `type` = "OrderByParam".tp, defaultValue = "null"),
                    ScalaClassParametr(name = "fetchSize", `type` = ScalaInt, defaultValue = "dataSource.Config.FetchSize"),
                    ScalaClassParametr(name = "dsRequest", `type` = "DSRequest".tp, defaultValue = "null")
                ),
                `type` = returnType,
                body = sb
            )

        def allColumnsP = allColumns.map(x => x + "=".space + x).mkString(",".space)

        def selectBody2(name: String, suffix: String = strEmpty) =
            ScalaBody(
                ScalaApplyObjectWithBody(name = name,
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "columns"),
                        ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"FromBo(${clazz.className.capitalize})"),
                        ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = "join"),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                        ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = "null"),
                        ScalaClassParametr(name = "orderBy", `type` = ScalaImplicitType, defaultValue = "orderBy"),
                        ScalaClassParametr(name = "fetchSize", `type` = ScalaImplicitType, defaultValue = "fetchSize"),
                        ScalaClassParametr(name = "dsRequest", `type` = ScalaImplicitType, defaultValue = "dsRequest")
                    ),
                    body = ScalaBody(ScalaCase(
                        ScalaCaseLine(expression =
                          ScalaExpressionBody(
                              ScalaBody(
                                  ScalaApplyObject(name = tupleType,
                                      parametrs = dsCaseClassParameters
                                  )
                              ),
                              serrializeToOneString = true
                          ), caseBody = ScalaBody(s"${productName}(${allColumnsP})")
                        )
                    ))
                )
            )


        def selectPMethod1(name: String, nameBodyMethod: String, returnType: AbstractScalaType) = ScalaMethod(
            name = name,
            generics = ScalaGenerics(ScalaGeneric("FT", ScalaUpperBound(ScalaBaseClassDeclare("Product".cls, ScalaAnyInGeneric("with FieldProduct"))))),
            parametrs = ScalaClassParametrs(
                ScalaClassParametr(name = "columns", `type` = "FT".tp, defaultValue = "allColumns"),
                ScalaClassParametr(name = "join", `type` = "JoinParam".tp, defaultValue = "null"),
                ScalaClassParametr(name = "where", `type` = "WhereParam".tp, defaultValue = "null"),
                ScalaClassParametr(name = "orderBy", `type` = "OrderByParam".tp, defaultValue = "null"),
                ScalaClassParametr(name = "fetchSize", `type` = ScalaInt, defaultValue = "dataSource.Config.FetchSize"),
                ScalaClassParametr(name = "dsRequest", `type` = "DSRequest".tp, defaultValue = "null")
            ),
            `type` = returnType,
            body = selectBody2(nameBodyMethod))

        def selectPOneMethod(returnType: AbstractScalaType, sb: => ScalaBody) =
            ScalaMethod(
                name = "selectPOne",
                parametrs = ScalaClassParametrs(
                    ScalaClassParametr(name = "join", `type` = "JoinParam".tp, defaultValue = "null"),
                    ScalaClassParametr(name = "where", `type` = "WhereParam".tp, defaultValue = "null")
                ),
                `type` = returnType,
                body = sb
            )

        def selectBody1(name: String, suffix: String = strEmpty) =
            ScalaBody(
                ScalaApplyObject(name = name,
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "allColumns"),
                        ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"FromBo(${clazz.className.capitalize})"),
                        ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = "join"),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                        ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = "null"),
                        ScalaClassParametr(name = "orderBy", `type` = ScalaImplicitType, defaultValue = "orderBy"),
                        ScalaClassParametr(name = "fetchSize", `type` = ScalaImplicitType, defaultValue = "fetchSize"),
                        ScalaClassParametr(name = "dsRequest", `type` = ScalaImplicitType, defaultValue = "dsRequest")
                    ),
                    suffix = suffix
                )
            )

        def selectPBody(name: String, suffix: String) = ScalaBody(
            ScalaCase(expression =
              ScalaExpressionBody(selectBody1(name = name, suffix = ".result"), serrializeToOneString = true),
                ScalaCaseLine(expression = "Success(list)".expr,
                    caseBody = ScalaBody(
                        ScalaApplyObject(name = "ValidationEx",
                            parametrs = ScalaClassParametrs(
                                ScalaClassParametr(name = strEmpty, `type` = ScalaImplicitType,
                                    defaultValue = ScalaApplyObject(name = "Success",
                                        parametrs = ScalaClassParametrs(
                                            ScalaClassParametr(name = strEmpty, `type` = ScalaImplicitType,
                                                defaultValue = ScalaBody(
                                                    ScalaApplyPartialObject(name = "list map",
                                                        body = ScalaCase(
                                                            ScalaCaseLine(expression =
                                                              ScalaExpressionBody(
                                                                  ScalaBody(
                                                                      ScalaApplyObject(name = tupleType,
                                                                          parametrs = dsCaseClassParameters
                                                                      )
                                                                  ),
                                                                  serrializeToOneString = true
                                                              ), caseBody = ScalaBody(s"${productName}(${allColumnsP})")
                                                            ),
                                                            ScalaCaseLine(
                                                                expression = "x".expr,
                                                                caseBody = "throw new RuntimeException (s\"mached as : $x\")".body
                                                            )
                                                        )
                                                    )
                                                ), onNewLine = true
                                            )
                                        ), suffix = suffix
                                    )
                                )
                            )
                        )
                    )
                ),
                ScalaCaseLine(expression = "Failure(x)".expr, caseBody = ScalaBody("ValidationEx(Failure(x))")),
                ScalaCaseLine(expression = "x".expr, caseBody = ScalaBody("throw new RuntimeException(s\"Bad branch. (${x})\")"))
            )
        )

        def selectOneBody1(suffix: String = strEmpty) =
            ScalaBody(
                ScalaApplyObject(name = "selectOneRoot",
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "allColumns"),
                        ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"FromBo(${clazz.className.capitalize})"),
                        ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = "join"),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                        ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = "null")
                    ),
                    suffix = suffix
                )
            )

        def selectPOneBody = ScalaBody(
            ScalaCase(expression =
              ScalaExpressionBody(selectOneBody1(suffix = ".result"), serrializeToOneString = true),
                ScalaCaseLine(expression = ScalaExpressionBody(
                    ScalaBody(
                        ScalaApplyObject(name = "Success",
                            parametrs = ScalaClassParametrs(
                                ScalaClassParametr(name = strEmpty, `type` = ScalaImplicitType,
                                    defaultValue = ScalaBody(
                                        ScalaApplyObject(name = tupleType,
                                            parametrs = dsCaseClassParameters
                                        )
                                    )
                                )
                            )
                        )
                    ),
                    serrializeToOneString = true),
                    caseBody = ScalaBody(
                        ScalaApplyObject(name = "ValidationEx",
                            parametrs = ScalaClassParametrs(
                                ScalaClassParametr(name = strEmpty, `type` = ScalaImplicitType,
                                    defaultValue = ScalaApplyObject(name = "Success",
                                        parametrs = ScalaClassParametrs(
                                            ScalaClassParametr(name = strEmpty, `type` = ScalaImplicitType,
                                                defaultValue = ScalaBody(s"${productName}(${allColumnsP})"),
                                                onNewLine = true
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                ),
                ScalaCaseLine(expression = "Failure(x)".expr, caseBody = ScalaBody("ValidationEx(Failure(x))")),
                ScalaCaseLine(expression = "x".expr, caseBody = ScalaBody("throw new RuntimeException(s\"Bad branch. (${x})\")"))
            ))

        dsClass addMembers(
          ScalaMethod(name = "this()", parametrsImplicit = ScalaClassParametrs(
              ScalaClassParametr(name = "dataSource", `type` = ScalaBoneCPDataSource, parametrType = ParametrImplicit)
          ), body = "this(SQLAlias(strEmpty))(dataSource)".body, serrializeToOneString = true),
          newLine,
          ScalaVariable(name = "quoted", serrializeToOneString = true, body = ScalaBody(quoted.toString)),
          newLine,
          ScalaMethod(name = "init",
              generics = ScalaGenerics(ScalaGeneric("FT", ScalaUpperBound(ScalaBaseClassDeclare("Product".cls, ScalaAnyInGeneric("with FieldProduct"))))),
              parametrs = ScalaClassParametrs(
                  ScalaClassParametr(name = "columns", `type` = "FT".tp, defaultValue = "null"),
                  ScalaClassParametr(name = "join", `type` = "JoinParam".tp, defaultValue = "null"),
                  ScalaClassParametr(name = "where", `type` = "WhereParam".tp, defaultValue = "null"),
                  ScalaClassParametr(name = "orderBy", `type` = "OrderByParam".tp, defaultValue = "null")
              ),
              `type` = className.tp, body = ScalaBody(
                  "_columns = columns",
                  "_join Join join",
                  "_where Where where",
                  "_orderBy OrderBy orderBy",
                  "this"
              )),
          newLine,
          ScalaComment(s"Class: ${clazz.className.bo}, group: ${clazz.group}"),
          newLine,
          ScalaVariable(name = clazz.className.capitalize, serrializeToOneString = true, body = ScalaBody( s"""new ${clazz.className.bo}${attrName.capitalize}(alias = alias + "B${j}".als)""")),
          newLine,
          ScalaVariable(name = "fromBO", serrializeToOneString = true, body = ScalaBody(clazz.className.capitalize)),
          newLine,
          ScalaComment("Fetch"),
          ScalaMethod(
              name = "selectList",
              generics = ScalaGenerics(ScalaGeneric("FT", ScalaUpperBound(ScalaBaseClassDeclare("Product".cls, ScalaAnyInGeneric("with FieldProduct"))))),
              parametrs = ScalaClassParametrs(
                  ScalaClassParametr(name = "columns", `type` = "FT".tp, defaultValue = "allColumns"),
                  ScalaClassParametr(name = "join", `type` = "JoinParam".tp, defaultValue = "null"),
                  ScalaClassParametr(name = "where", `type` = "WhereParam".tp, defaultValue = "null"),
                  ScalaClassParametr(name = "orderBy", `type` = "OrderByParam".tp, defaultValue = "null"),
                  ScalaClassParametr(name = "fetchSize", `type` = ScalaInt, defaultValue = "dataSource.Config.FetchSize"),
                  ScalaClassParametr(name = "dsRequest", `type` = "DSRequest".tp, defaultValue = "null")
              ),
              `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", ScalaGenerics("FT#ReturnType")))),
              body = ScalaBody(
                  ScalaApplyObject(name = "selectListRoot",
                      parametrs = ScalaClassParametrs(
                          ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "columns"),
                          ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"FromBo(${clazz.className.capitalize})"),
                          ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = "join"),
                          ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                          ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = "null"),
                          ScalaClassParametr(name = "orderBy", `type` = ScalaImplicitType, defaultValue = "orderBy"),
                          ScalaClassParametr(name = "fetchSize", `type` = ScalaImplicitType, defaultValue = "fetchSize"),
                          ScalaClassParametr(name = "dsRequest", `type` = ScalaImplicitType, defaultValue = "dsRequest"))
                  )
              ),
              serrializeToOneString = false
          ),
          newLine,
          ScalaMethod(
              name = "selectIterator",
              generics = ScalaGenerics(ScalaGeneric("FT", ScalaUpperBound(ScalaBaseClassDeclare("Product".cls, ScalaAnyInGeneric("with FieldProduct"))))),
              parametrs = ScalaClassParametrs(
                  ScalaClassParametr(name = "columns", `type` = "FT".tp, defaultValue = "allColumns"),
                  ScalaClassParametr(name = "join", `type` = "JoinParam".tp, defaultValue = "null"),
                  ScalaClassParametr(name = "where", `type` = "WhereParam".tp, defaultValue = "null"),
                  ScalaClassParametr(name = "orderBy", `type` = "OrderByParam".tp, defaultValue = "null"),
                  ScalaClassParametr(name = "fetchSize", `type` = ScalaInt, defaultValue = "dataSource.Config.FetchSize"),
                  ScalaClassParametr(name = "dsRequest", `type` = "DSRequest".tp, defaultValue = "null")
              ),
              `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationExIterator".cls, ScalaGeneric("Iterator", ScalaGenerics("FT#ReturnType")))),
              body = ScalaBody(
                  ScalaApplyObject(name = "selectIteratorRoot",
                      parametrs = ScalaClassParametrs(
                          ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "columns"),
                          ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"FromBo(${clazz.className.capitalize})"),
                          ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = "join"),
                          ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                          ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = "null"),
                          ScalaClassParametr(name = "orderBy", `type` = ScalaImplicitType, defaultValue = "orderBy"),
                          ScalaClassParametr(name = "fetchSize", `type` = ScalaImplicitType, defaultValue = "fetchSize"),
                          ScalaClassParametr(name = "dsRequest", `type` = ScalaImplicitType, defaultValue = "dsRequest"))
                  )
              ),
              serrializeToOneString = false
          ),
          newLine,
          ScalaMethod(
              name = "selectOne",
              generics = ScalaGenerics(ScalaGeneric("FT", ScalaUpperBound(ScalaBaseClassDeclare("Product".cls, ScalaAnyInGeneric("with FieldProduct"))))),
              parametrs = ScalaClassParametrs(
                  ScalaClassParametr(name = "columns", `type` = "FT".tp, defaultValue = "allColumns"),
                  ScalaClassParametr(name = "join", `type` = "JoinParam".tp, defaultValue = "null"),
                  ScalaClassParametr(name = "where", `type` = "WhereParam".tp, defaultValue = "null")
              ),
              `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("FT#ReturnType"))),
              body = ScalaBody(
                  ScalaApplyObject(name = "selectOneRoot",
                      parametrs = ScalaClassParametrs(
                          ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "columns"),
                          ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"FromBo(${clazz.className.capitalize})"),
                          ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = "join"),
                          ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                          ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = "null")
                      )
                  )
              ),
              serrializeToOneString = false
          ),
          ScalaEndComment("Fetch"))

        dsClass addMember newLine
        dsClass addMember
          ScalaMethod(name = "preparedBO4Wrap",
              parametrs = ScalaClassParametrs(
                  ScalaClassParametr(name = "columns", `type` = "List[BasicClassBOColumn[_]]".tp)
              ),
              `type` = "PreparedResult".tp,
              body = ScalaBody(
                  ScalaApplyObject(
                      name = "prepareSelect",
                      parametrs = ScalaClassParametrs(
                          ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = "_join"),
                          ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "_where"),
                          ScalaClassParametr(name = "orderBy", `type` = ScalaImplicitType, defaultValue = "_orderBy"),
                          ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = "null"),
                          ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"FromBo(${clazz.className.capitalize})"),
                          ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue =
                            ScalaBody(ScalaCase(
                                expression = "_columns".expr,
                                ScalaCaseLine(expression = "null".expr, caseBody = "allColumns.fields.toSet intersect columns.toSet".body, serrializeToOneString = true),
                                ScalaCaseLine(expression = "_".expr, caseBody = "_columns.fields.toSet intersect columns.toSet".body, serrializeToOneString = true)
                            ))
                          )
                      )
                  )),
              serrializeToOneString = false
          )

        val dsObject = new ScalaClassDeclare {
            scalaClassGen = className.cls
            typeScalaClass = TypeScalaObject
        }

        val dsCaseClass = new ScalaClassDeclare {
            scalaClassGen = (className + "Data").cls
            typeScalaClass = TypeScalaCaseClass
        }

        dsObject addMembers(
          ScalaMethod(name = "apply",
              parametrsImplicit = ScalaClassParametrs(ScalaClassParametr(name = "dataSource", `type` = ScalaBoneCPDataSource, parametrType = ParametrImplicit)), serrializeToOneString = true, body = ScalaBody(s"new ${className}(alias = SQLAlias(strEmpty))")
          ),
          ScalaMethod(name = "apply",
              parametrs = ScalaClassParametrs(
                  ScalaClassParametr(name = "alias", `type` = "SQLAlias".tp)
              ),
              parametrsImplicit = ScalaClassParametrs(
                  ScalaClassParametr(name = "dataSource", `type` = ScalaBoneCPDataSource, parametrType = ParametrImplicit)
              ),
              serrializeToOneString = true, body = ScalaBody(s"new ${className}")),
          newLine,
          ScalaVariable(name = "objectName", serrializeToOneString = true, body = ScalaBody(clazz.className.dblQuoted)),
          ScalaVariable(name = "groupName", serrializeToOneString = true, body = ScalaBody(clazz.group.dblQuoted))
        )

        j += 1
        var columnTypes = ""
        var columnTypes1 = ""
        var forTupleTemplate = strEmpty

        var i = 0
        var i1 = 0

        import com.simplesys.common.equality.TypeWiseBalancedEquality._
        def isEnumClass(c: IClass) = schema.enumClasses.filter {
            case x: IClass => x === c
        }.length > 0

        val elements = ArrayBuffer.empty[ScalaElement]

        def addAttrs(_fkName: String, _attrs: Seq[AttrDef[_]], _clazz: IClass, selffAttrs: Boolean = true, aliasBo: String = "", add2AllColumns: Boolean = true) {
            val attrs = _attrs.toArray
            Sorting.quickSort(attrs)(AttrDefOrd)

            attrs.foreach {
                attr =>
                    val fieldName = s"${attr.name}${_clazz.className.capitalize}" + {
                        if (_fkName.isEmpty) strEmpty else s"_${_fkName}"
                    }

                    val fieldName4TupleTemplate = s"${attr.name}${_clazz.className.capitalize}"

                    val _attrType = attr.attrType.scalaTypeAsString(clazz.group, schema)
                    val attrType = s"${if (attr.isMandatory) _attrType else s"Array[${_attrType}]"}"

                    if (add2AllColumns)
                        if (i < maxArity) {
                            if (!allColumns.exists(_.trim === fieldName)) {
                                allColumns += fieldName.space
                                columnTypes += attrType + ",".space
                                forTupleTemplate += fieldName4TupleTemplate + ":".space + attrType + ",".space
                                dsCaseClassParameters += ScalaClassParametr(name = fieldName, `type` = attrType.tp)
                                i += 1
                            } else {
                                logger debug s"double attr: ${fieldName}"
                            }

                            if (selffAttrs) {
                                columnTypes1 += attrType + ",".space
                                i1 += 1
                            }
                        }

                    if (elements.filter {
                        case element: ScalaVariable if element.name === fieldName => true
                        case _ => false
                    }.length === 0) {
                        elements += ScalaVariable(name = fieldName, serrializeToOneString = true, body = ScalaBody(s"${_clazz.className.capitalize}${if (_fkName.isEmpty) strEmpty else s"_${_fkName}"}.${attr.name}"))

                        if (!selffAttrs && aliasBo.nonEmpty)
                            elements ++= Seq( s"""${fieldName} As ${fieldName}.name + "_${aliasBo}" """, newLine, newLine)
                    }
            }
        }
        addAttrs(_fkName = strEmpty, _attrs = attrs, _clazz = clazz)

        dsClass addMembers (elements: _*)

        val classes = ArrayBuffer((clazz, strEmpty))

        dsClass addMember ScalaEndComment(s"Class: ${clazz.className.bo}, group: ${clazz.group}")

        val fks = clazz.fks.toArray
        Sorting.quickSort(fks)(ForeignKeyConstraintDefOrd)

        dsClass addMembers (ScalaVariable(name = "boCount", body = if (!forLob) s"${fks.length + 1}".body else "1".body, serrializeToOneString = true, `override` = OverrideMofificator))

        if (!forLob) {
            fks foreach {
                case fk: ForeignKeyConstraintDef =>
                    val fkClass: IClass = fk.referencedClassRef.toClass
                    if (fkClass.className != clazz.className || fkClass.group != clazz.group) {
                        classes += (fkClass -> fk.softNameForCompare)

                        if (fkClass.group != clazz.group) {
                            addImports += s"${packageName}.${fkClass.group}.${if (isEnumClass(fkClass)) fkClass.className.enum else fkClass.className.bo}".imp
                            addImports += s"${packageName}.${fkClass.group}.${fkClass.className}".imp
                        }

                        elements.clear()
                        addAttrs(_fkName = fk.softNameForCompare, _attrs = fkClass.pk.attrs, _clazz = fkClass, selffAttrs = false, aliasBo = s"B${j}")
                        addAttrs(_fkName = fk.softNameForCompare, _attrs = fkClass.defaultSettings.showAttrsResult, _clazz = fkClass, selffAttrs = false, aliasBo = s"B${j}")

                        import com.simplesys.common.equality.SimpleEquality._

                        fkClass.ucs.filter(_.ucType !== PK) foreach {
                            _uc =>
                                addAttrs(_fkName = fk.softNameForCompare, _attrs = _uc.attrs, _clazz = fkClass, selffAttrs = false, aliasBo = s"B${j}", add2AllColumns = false)
                        }

                        fkClass.fks foreach {
                            _fk =>
                                addAttrs(_fkName = fk.softNameForCompare, _attrs = _fk.attrs, _clazz = fkClass, selffAttrs = false, aliasBo = s"B${j}", add2AllColumns = false)
                        }
                        dsClass addMembers(
                          newLine,
                          ScalaComment(s"Class: ${fkClass.className}Bo_${fk.softNameForCompare}, group: ${fkClass.group}")
                        )
                        if (elements.length > 0) {
                            dsClass addMember
                              ScalaVariable(name = s"${fkClass.className.capitalize}_${fk.softNameForCompare}", serrializeToOneString = true, body = ScalaBody( s"""new ${if (isEnumClass(fkClass)) fkClass.className.enum else fkClass.className.bo}(alias = alias +"B${j}".als)"""))

                            j += 1

                            dsClass addMembers (elements: _*)

                            dsClass addMember
                              (s"_join.${if (fk.isMandatory) "InnerJoin" else "LeftJoin"}(${fkClass.className.capitalize + "_" + fk.softNameForCompare})" + (fk.attrMappingAttrDefs map {
                                  case (currCl, refCl) => s"(${currCl.name}${clazz.className.capitalize} === ${refCl.name}${fkClass.className.capitalize}_${fk.softNameForCompare}).JoinCondition"
                              }).mkString("(", space + "And".space, ")"))
                        }
                        dsClass addMember ScalaEndComment(s"Class: ${fkClass.className}Bo_${fk.softNameForCompare}, group: ${fkClass.group}")
                    }
            }
        }

        //Sorting.quickSort(fks)(ForeignKeyConstraintDefOrd)


        dsClass.getConstraints(classes, forLob)

        columnTypes = "TupleSS" + i + "[" + columnTypes.delLastChar + "]"
        columnTypes1 = "TupleSS" + i1 + "[" + columnTypes1.delLastChar + "]"
        tupleType = "TupleSS" + i

        forTupleTemplate = if (i > 0) forTupleTemplate.delLastChar else forTupleTemplate.trim

        dsClass addMembers(
          newLine,
          ScalaShortComment(s"For select tuple: ($forTupleTemplate)"),
          newLine,
          ScalaAliasType(name = "ColumnTypes", body = ScalaBody(columnTypes)),
          ScalaMethod(name = "allColumns", serrializeToOneString = true, body = ScalaBody(allColumns.mkString(space + "~".space))),
          ScalaMethod(name = "allColumns1", serrializeToOneString = true, body = ScalaBody(s"Seq(${allColumns.mkString(space + ",".space)})")),
          newLine,
          ScalaComment("Fetch Product"),
          selectPMethod(name = "selectPList", ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", ScalaGenerics(productName)))), sb = selectPBody(name = "selectListRoot", suffix = strEmpty)),
          newLine,
          selectPMethod1("selectPIterator", "selectPIteratorRoot", ScalaClassGenericType(ScalaBaseClassDeclare("ValidationExIterator".cls, ScalaGeneric("Iterator", ScalaGenerics(productName))))),
          newLine,
          selectPOneMethod(ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric(s"${productName}"))), sb = selectPOneBody),
          ScalaEndComment("Fetch Product")
        )

        if (!clazz.isAbstract)
            dsClass addMembers(
              newLine,
              ScalaComment("insert"),
              ScalaMethod(
                  name = "insert",
                  body = s"${clazz.className.capitalize}.insert(values: _*)".body,
                  serrializeToOneString = true,
                  parametrs = ScalaClassParametrs(ScalaClassParametr(name = "values", `type` = (columnTypes1 + "*").tp)),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", "Int")))),
              ScalaMethod(
                  name = "insertP",
                  body = s"${clazz.className.capitalize}.insertP(values: _*)".body,
                  serrializeToOneString = true,
                  parametrs = ScalaClassParametrs(ScalaClassParametr(name = "values", `type` = s"${clazz.className.capitalize}${attrName.capitalize}*".tp)),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", "Int")))),
              newLine,
              ScalaMethod(
                  name = "insertWithoutCommit",
                  body = s"${clazz.className.capitalize}.insertWithoutCommit(connection = connection, values: _*)".body,
                  serrializeToOneString = true,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "connection", `type` = "Connection".tp),
                      ScalaClassParametr(name = "values", `type` = (columnTypes1 + "*").tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("List".cls, ScalaGeneric("Int")))),
              ScalaMethod(
                  name = "insertPWithoutCommit",
                  serrializeToOneString = true,
                  body = s"${clazz.className.capitalize}.insertPWithoutCommit(connection = connection, values: _*)".body,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "connection", `type` = "Connection".tp),
                      ScalaClassParametr(name = "values", `type` = s"${clazz.className.capitalize}${attrName.capitalize}*".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("List".cls, ScalaGeneric("Int")))),
              ScalaEndComment("insert"),
              newLine,
              ScalaComment("update"),
              ScalaMethod(
                  name = "update",
                  serrializeToOneString = true,
                  body = s"${clazz.className.capitalize}.update(value = values, where = where)".body,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "values", `type` = columnTypes1.tp),
                      ScalaClassParametr(name = "where", `type` = "WhereParam".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", "Int")))),
              ScalaMethod(
                  name = "updateP",
                  serrializeToOneString = true,
                  body = s"${clazz.className.capitalize}.updateP(value = values, where = where)".body,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "values", `type` = s"${clazz.className.capitalize}${attrName.capitalize}".tp),
                      ScalaClassParametr(name = "where", `type` = "WhereParam".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", "Int")))),
              newLine,
              ScalaMethod(
                  name = "updateWithoutCommit",
                  serrializeToOneString = true,
                  body = s"${clazz.className.capitalize}.updateWithoutCommit(connection = connection, value = values, where = where)".body,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "connection", `type` = "Connection".tp),
                      ScalaClassParametr(name = "values", `type` = columnTypes1.tp),
                      ScalaClassParametr(name = "where", `type` = "WhereParam".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("List".cls, ScalaGeneric("Int")))),
              ScalaMethod(
                  name = "updatePWithoutCommit",
                  serrializeToOneString = true,
                  body = s"${clazz.className.capitalize}.updatePWithoutCommit(connection = connection, value = values, where = where)".body,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "connection", `type` = "Connection".tp),
                      ScalaClassParametr(name = "values", `type` = s"${clazz.className.capitalize}${attrName.capitalize}".tp),
                      ScalaClassParametr(name = "where", `type` = "WhereParam".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("List".cls, ScalaGeneric("Int")))),
              ScalaEndComment("update"),
              newLine,
              ScalaComment("delete"),
              ScalaMethod(
                  name = "delete",
                  serrializeToOneString = true,
                  body = s"${clazz.className.capitalize}.delete(where = where)".body,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "where", `type` = "WhereParam".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", "Int")))),
              ScalaMethod(
                  name = "deleteWithoutCommit",
                  serrializeToOneString = true,
                  body = s"${clazz.className.capitalize}.deleteWithoutCommit(connection = connection, where = where)".body,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "connection", `type` = "Connection".tp),
                      ScalaClassParametr(name = "where", `type` = "WhereParam".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("List".cls, ScalaGeneric("Int")))),
              ScalaEndComment("Delete"),
              newLine
            )

        val module = ScalaModule(
            packageName.pkg,
            clazz.group.pkg,
            newLine,
            "com.simplesys.jdbc.control.dataSet.DataSet".imp,
            "com.simplesys.jdbc.control.ValidationEx".imp,
            "java.sql.Connection".imp,
            "org.joda.time.{LocalDateTime, DateTime}".imp,
            "com.simplesys.oracle.pool.PoolDataSource".imp,
            "com.simplesys.SQL._".imp,
            "com.simplesys.jdbc._".imp,
            "com.simplesys.SQL.Gen.{SQLAbsTable, SQLAlias}".imp,
            "com.simplesys.common.Strings._".imp,
            "com.simplesys.jdbc.control.classBO.{OrderBy, WheresList, FromBo, LeftJoin}".imp,
            "com.simplesys.jdbc.control.classBO.JoinsBo._".imp,
            "com.simplesys.jdbc.control.SuperTuple1.FieldProduct".imp,
            "collection.SeqView".imp,
            "scalaz.{Failure, Success}".imp,
            "scalaz.Validation._".imp,
            "com.simplesys.jdbc.control.clob._".imp,
            "com.simplesys.tuple._".imp,
            "com.simplesys.jdbc.control._".imp)

        module ++= addImports.toArray.distinct

        dsCaseClass setParametrs dsCaseClassParameters
        module ++= (
          newLine,
          dsCaseClass,
          newLine,
          dsObject,
          newLine,
          dsClass)


        //module.log

        res <== {
            out =>
                out(genMessageCreating("GenDSs"))
                out(newLine)
                out(newLine)
                out(module.serrialize())
        }
    }

    def createSeq: Seq[File] = {
        logger info (s"Begin #938.")
        val classes = (schema.simpleClasses ++ schema.hierarchyClasses) toArray

        Sorting.quickSort(classes)(IClassOrd)

        val res = ArrayBuffer.empty[File]

        classes foreach (res ++= genDSs(_))
        logger info (s"Done #938.")
        res
    }
}
