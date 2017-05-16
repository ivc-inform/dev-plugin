package ru.simplesys.plugins.sourcegen.app.Gen

import scalax.file.{PathSet, Path}
import com.simplesys.io._
import java.io.File
import sbt.Logger
import ru.simplesys.plugins.sourcegen.meta._
import com.simplesys.common.Strings._
import scala.util.Sorting
import com.simplesys.genSources._
import com.simplesys.common._
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.scalaGen._
import ru.simplesys.plugins.sourcegen.app._
import java.net.URI
import scala.collection.mutable.ArrayBuffer

class GenEnums(val appFilePath: Path,
               val outFilePath: Path,
               val packageName: String,
               val pkgBOName: String,
               val quoted: Boolean,
               val stage: String,
               val logger: Logger) extends GenScala1 with Log {

    val schemaPath: URI = "".xsdURI

    val sourceBOFiles: PathSet[Path] = appFilePath * "*.xml"
    implicit val schema = SchemaDef(pkgBOName, sourceBOFiles.files)

    def create: File = ????

    private def genEnum(clazz: IEnumClass): File = {

        val className = clazz.className.enum

        implicit val discriminatorSeq = clazz.discriminatorColumnWVals

        val res: File = (outFilePath / clazz.group / (className + ".scala")).createFile(failIfExists = false).toFile

        val enumClass = new ScalaClassDeclare {
            scalaClassGen = className.cls
            parametrs = ScalaClassParametrs(
                ScalaClassParametr(name = "alias", `type` = "SQLAlias".tp, parametrType = ParametrVal, defaultValue = strEmpty)
            )
            parametrsImplicit = ScalaClassParametrs(
                ScalaClassParametr(name = "dataSource", `type` = ScalaBoneCPDataSource, parametrType = ParametrImplicitVal)
            )
            extensibleClass = ScalaClassGenericExtensible(new ScalaBaseClassDeclare {
                scalaClassGen = "ClassBO".cls
                generics = ScalaGeneric(className)
            })
        }

        val productName = clazz.className
        val boProductObjectParametrs = ScalaClassParametrs()

        val enumObject = new ScalaClassDeclare {
            scalaClassGen = className.cls
            typeScalaClass = TypeScalaObject
        }

        enumObject addMembers(
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
              serrializeToOneString = true, body = ScalaBody(s"new ${className}(alias = alias)")),
          newLine,
          ScalaVariable(name = "objectName", serrializeToOneString = true, body = ScalaBody(clazz.className.dblQuoted)),
          ScalaVariable(name = "groupName", serrializeToOneString = true, body = ScalaBody(clazz.group.dblQuoted))
          )

        enumClass addMembers(
          newLine,
          ScalaComment(s"Class: ${clazz.className}, group: ${clazz.group}"),
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
          ScalaVariable(name = "quoted", serrializeToOneString = true, body = ScalaBody(quoted.toString)),
          newLine
          )

        val attrs = clazz.attrs.toArray
        Sorting.quickSort(attrs)(AttrDefOrd)

        val tables = clazz.linkRefsToAllTables.map(_.toTable)

        var allColumns = ""
        var allColumnsP = ""
        var forTuple = ""
        var columnTypes = ""
        var tupleType = ""
        var i = 0

        enumClass addMember ScalaComment("Tables")
        tables foreach {
            table =>
                val tableVal = s"${table.tableName.tbl}"
                enumClass addMember
                  ScalaVariable(name = s"${tableVal}", body = ScalaBody(
                      ScalaApplyAnonimousClass(
                          name = s"""${tableVal}(alias = alias + "T${i}".als)"""
                      )), serrializeToOneString = true)
                i += 1

        }

        i = 0
        enumClass addMembers(ScalaEndComment("Tables"), newLine)

        enumClass addMembers (ScalaComment("Columns for select"))
        attrs foreach {
            attr =>
                i += 1

                val column = attr.singleColumnsRef.toCol
                val table = column.tableRef.toTable

                val tableVal = s"${table.tableName.tbl}"
                val columnVal = column.scalaName
                val tblColumn = tableVal + "." + (if (attr.isMandatory) column.scalaName else column.scalaName + "Option")

                allColumns += columnVal.space + "~".space

                if (!attr.isCalculated && !attr.isDiscriminator)
                    allColumnsP += columnVal + " = " + columnVal + ",".space

                val _columnType = if (!column.dataType.isComplexDataType) column.dataType.scalaTypeAsString(clazz.group, schema) else clazz.className
                val columnType = s"${if (column.isMandatory) _columnType else s"Array[${_columnType}]"}"

                forTuple += column.scalaName + ":".space + columnType + ",".space
                boProductObjectParametrs += ScalaClassParametr(name = column.scalaName, `type` = columnType.tp)
                columnTypes += columnType + ",".space

                val typeColumn = s"${if (!column.dataType.isComplexDataType) column.dataType.scalaTypeAsString(clazz.group, schema) else s"Enum${column.dataType.simpleScalaType}"}${if (column.isMandatory) "" else "Option"}Column"

                enumClass addMember
                  ScalaVariable(
                      name = columnVal,
                      body = if (!column.dataType.isComplexDataType) ScalaBody(ScalaApplyObject(
                          name = typeColumn,
                          parametrs = ScalaClassParametrs(
                              ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = column.dbName.dblQuoted),
                              ScalaClassParametr(name = "nameInBo", `type` = ScalaImplicitType, defaultValue = (clazz.group + "_" + clazz.className + "_" + attr.name).dblQuoted),
                              ScalaClassParametr(name = "tableColumn", `type` = ScalaImplicitType, defaultValue = tblColumn)
                          )))
                      else
                          ScalaBody(
                              new ScalaClassDeclare {
                                  scalaClassGen = typeColumn.cls
                                  typeScalaClass = AnonimousScalaClass
                                  generics = ScalaGenerics(clazz.className)
                                  parametrs = ScalaClassParametrs(
                                      ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = column.dbName.dblQuoted),
                                      ScalaClassParametr(name = "nameInBo", `type` = ScalaImplicitType, defaultValue = (clazz.group + "_" + clazz.className + "_" + attr.name).dblQuoted),
                                      ScalaClassParametr(name = "tableColumn", `type` = ScalaImplicitType, defaultValue = tblColumn)
                                  )
                                  members = ArrayBuffer(
                                      ScalaMethod(name = "default", body = ScalaBody(s"${clazz.className}.default"), serrializeToOneString = true)
                                  )
                              }
                          ),
                      serrializeToOneString = !column.dataType.isComplexDataType)
        }

        tupleType = "TupleSS" + i
        columnTypes = "TupleSS" + i + "[" + columnTypes.delLastChar + "]"
        allColumns = if (i > 0) allColumns.delLastChar else allColumns.trim
        allColumnsP = allColumnsP.delLastChar
        forTuple = if (i > 0) forTuple.delLastChar else forTuple.trim

        enumClass addMembers(
          newLine,
          ScalaShortComment(s"For select tuple: (${forTuple})"),
          newLine,
          ScalaAliasType(name = "ColumnTypes", body = ScalaBody(columnTypes)),
          ScalaMethod(name = "allColumns", serrializeToOneString = true, body = ScalaBody(allColumns)),
          newLine
          )

        enumClass getConstraints clazz
        enumClass addMembers(ScalaEndComment("Columns for select"), newLine)

        enumClass addMember ScalaComment("Columns for Insert/Update/Delete")

        var insertPartOfBody = ScalaControlBody(expression = "connection".expr, "")
        var insertPartOfBody1 = ScalaControlBody()

        var deletePartOfBody = ScalaControlBody(expression = "connection".expr, "")
        var deletePartOfBody1 = ScalaControlBody()

        var k = 1

        def getColumnsIndex(column: ColumnDef[_], table: ITable): Int = {
            import scala.util.control.Breaks._
            var res = 0
            var m = false

            breakable {
                val a: Array[Seq[ColumnDef[_]]] = attrs.map(_.columns)
                a foreach {
                    seq =>
                        res += 1
                        m = false
                        seq foreach {
                            _column =>
                                if (_column.scalaName === column.scalaName && _column.tableRef.toTable === column.tableRef.toTable)
                                    m = true

                        }
                        if (m)
                            break
                }
            }
            if (m) res else 0
        }

        tables foreach {
            table =>
                val tableVal = table.tableName.tbl

                val columns = table.columns.toArray
                Sorting.quickSort(columns)(ColumnDefOrd)

                enumClass addMember ScalaComment(s"Table: ${table.tableName}")
                var allColumns = ""
                var columnTypes = ""
                var seqInsertValues = ""
                var seqUpdateValues = ""
                var seqInsertPValues = ""
                var seqUpdatePValues = ""
                var i = 0
                columns foreach {
                    column =>
                        val columnVal = column.scalaName + tableVal
                        val tblColumn = tableVal + "." + (if (column.isMandatory) column.scalaName else column.scalaName + "Option")

                        val idx = getColumnsIndex(column, table)
                        if (idx > 0) {
                            allColumns += columnVal.space + "~".space

                            seqInsertValues += s"Insert(${columnVal}, value._${idx})".space
                            seqInsertPValues += s"Insert(${columnVal}, value.${column.scalaName})".space
                            val columnType = column.dataType.simpleDataType.scalaTypeAsString(table.group, schema)
                            columnTypes += s"${if (column.isMandatory) columnType else "Array[" + columnType + "]"}" + ",".space
                            i += 1

                            seqUpdateValues += s"Set(${column.scalaName}, value._${idx})".space
                            seqUpdatePValues += s"Set(${column.scalaName}, value.${column.scalaName})".space

                            enumClass addMember
                              ScalaVariable(
                                  name = columnVal,
                                  body = ScalaBody(s"${tblColumn}"),
                                  serrializeToOneString = true)
                        }
                }

                columnTypes = "TupleSS" + i + "[" + columnTypes.delLastChar + "]"
                allColumns = if (columns.length > 1) allColumns.delLastChar else allColumns.trim

                seqInsertValues = seqInsertValues.trim
                seqUpdateValues = seqUpdateValues.trim

                seqInsertPValues = seqInsertPValues.trim
                seqUpdatePValues = seqUpdatePValues.trim

                val insert = ScalaApplyObject(name = "deleteWithoutCommit", parametrs = ScalaClassParametrs(
                    ScalaClassParametr(name = "connection", `type` = ScalaImplicitType, defaultValue = "connection"))
                )

                val insert1 = ScalaApplyObject(name = s"${tableVal}.insertWithoutCommit", parametrs = ScalaClassParametrs(
                    ScalaClassParametr(name = "connection", `type` = ScalaImplicitType, defaultValue = "connection"),
                    ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = allColumns),
                    ScalaClassParametr(name = "", `type` = ScalaImplicitType, onNewLine = true, defaultValue = ScalaControlStruct(
                        name = "values map",
                        body = ScalaControlBody(expression = "value".expr, seqInsertPValues)
                    )))
                )

                insertPartOfBody ++=(insert, newLine, insert1)
                insertPartOfBody1 ++=(insert, newLine, insert1)

                if (k < tables.length) {
                    insertPartOfBody += ScalaPlusPlusSymbol
                    insertPartOfBody1 += ScalaPlusPlusSymbol
                }
                k += 1

                enumClass addMember ScalaEndComment(s"Table: ${table.tableName}")
        }

        k = 1
        tables.reverse foreach {
            table =>
                val delete = ScalaApplyObject(
                    name = "deleteWithoutCommit",
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "connection", `type` = ScalaImplicitType, defaultValue = "connection"),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "null"),
                        ScalaClassParametr(name = "table", `type` = ScalaImplicitType, defaultValue = s"SQLTable(name = ${table.tableName.tbl}.databaseTablename)")
                    )
                )

                deletePartOfBody += delete
                deletePartOfBody1 += delete

                if (k < tables.length) {
                    deletePartOfBody += ScalaPlusPlusSymbol
                    deletePartOfBody1 += ScalaPlusPlusSymbol
                }
                k += 1
        }

        enumClass addMembers(ScalaEndComment("Columns for Insert/Update/Delete"), newLine)

        var discriminators = ""
        discriminatorSeq.zipWithIndex foreach {
            case (discriminator, index) => discriminators += s"${if (index === 0) "Where" else "And"}(${discriminator.colRef.name} === ${if (discriminator.colRef.toCol.dataType.simpleDataType.toString === "String") discriminator.value.dblQuoted else discriminator.value})".space
        }

        var joins = ""
        clazz.columnRelationsLinkAllTables foreach {
            relation => joins += s"InnerJoin(${relation.columnFrom.name + relation.columnFrom.table.tableName.tbl} === ${relation.columnTo.name + relation.columnTo.table.tableName.tbl})".space
        }

        joins = {
            if (joins.isEmpty) space + "join" else joins.trim + space + "InnerJoin(join)"
        }

        def selectBody(suffix: String = "") =
            ScalaBody(
                ScalaApplyObject(name = "selectListRoot",
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "columns"),
                        ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"From(${tables.head.tableName.tbl})"),
                        ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = joins),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                        ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = if (discriminators.trim.isEmpty) "null" else discriminators.trim),
                        ScalaClassParametr(name = "orderBy", `type` = ScalaImplicitType, defaultValue = "orderBy"),
                        ScalaClassParametr(name = "dsRequest", `type` = ScalaImplicitType, defaultValue = "null")
                    ),
                    suffix = suffix
                )
            )

        def selectOneBody(suffix: String = "") =
            ScalaBody(
                ScalaApplyObject(name = "selectOneRoot",
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "columns"),
                        ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"From(${tables.head.tableName.tbl})"),
                        ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = joins),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                        ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = if (discriminators.trim.isEmpty) "null" else discriminators.trim)
                    ),
                    suffix = suffix
                )
            )

        def selectBody1(suffix: String = "") =
            ScalaBody(
                ScalaApplyObject(name = "selectListRoot",
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "allColumns"),
                        ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"From(${tables.head.tableName.tbl})"),
                        ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = joins),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                        ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = if (discriminators.trim.isEmpty) "null" else discriminators.trim),
                        ScalaClassParametr(name = "orderBy", `type` = ScalaImplicitType, defaultValue = "orderBy"),
                        ScalaClassParametr(name = "dsRequest", `type` = ScalaImplicitType, defaultValue = "null")
                    ),
                    suffix = suffix
                )
            )

        def selectOneBody1(suffix: String = "") =
            ScalaBody(
                ScalaApplyObject(name = "selectOneRoot",
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "allColumns"),
                        ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"From(${tables.head.tableName.tbl})"),
                        ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = joins),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                        ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = if (discriminators.trim.isEmpty) "null" else discriminators.trim)
                    ),
                    suffix = suffix
                )
            )

        def selectMethod(returnType: AbstractScalaType) = ScalaMethod(
            name = "selectList",
            generics = ScalaGenerics(ScalaGeneric("FT", ScalaUpperBound(ScalaBaseClassDeclare("Product".cls, ScalaAnyInGeneric("with FieldProduct"))))),
            parametrs = ScalaClassParametrs(
                ScalaClassParametr(name = "columns", `type` = "FT".tp, defaultValue = "allColumns"),
                ScalaClassParametr(name = "join", `type` = "JoinParam".tp, defaultValue = "null"),
                ScalaClassParametr(name = "where", `type` = "WhereParam".tp, defaultValue = "null"),
                ScalaClassParametr(name = "orderBy", `type` = "OrderByParam".tp, defaultValue = "null"),
                ScalaClassParametr(name = "dsRequest", `type` = "DSRequest".tp, defaultValue = "null")
            ),
            `type` = returnType,
            body = selectBody())

        def selectOneMethod(returnType: AbstractScalaType) = ScalaMethod(
            name = "selectOne",
            generics = ScalaGenerics(ScalaGeneric("FT", ScalaUpperBound(ScalaBaseClassDeclare("Product".cls, ScalaAnyInGeneric("with FieldProduct"))))),
            parametrs = ScalaClassParametrs(
                ScalaClassParametr(name = "columns", `type` = "FT".tp, defaultValue = "allColumns"),
                ScalaClassParametr(name = "join", `type` = "JoinParam".tp, defaultValue = "null"),
                ScalaClassParametr(name = "where", `type` = "WhereParam".tp, defaultValue = "null")
            ),
            `type` = returnType,
            body = selectOneBody())

        def selectPMethod(returnType: AbstractScalaType, sb: => ScalaBody) =
            ScalaMethod(
                name = "selectList",
                parametrs = ScalaClassParametrs(
                    ScalaClassParametr(name = "join", `type` = "JoinParam".tp, defaultValue = "null"),
                    ScalaClassParametr(name = "where", `type` = "WhereParam".tp, defaultValue = "null"),
                    ScalaClassParametr(name = "orderBy", `type` = "OrderByParam".tp, defaultValue = "null")
                ),
                `type` = returnType,
                body = sb
            )

        def selectPOneMethod(returnType: AbstractScalaType, sb: => ScalaBody) =
            ScalaMethod(
                name = "SelectOne",
                parametrs = ScalaClassParametrs(
                    ScalaClassParametr(name = "join", `type` = "JoinParam".tp, defaultValue = "null"),
                    ScalaClassParametr(name = "where", `type` = "WhereParam".tp, defaultValue = "null")
                ),
                `type` = returnType,
                body = sb
            )

        def selectPBody = ScalaBody(
            ScalaCase(expression =
              ScalaExpressionBody(selectBody1(suffix = ".result"), serrializeToOneString = true),
                ScalaCaseLine(expression = "Success(list)".expr,
                    caseBody = ScalaBody(
                        ScalaApplyObject(name = "ValidationEx",
                            parametrs = ScalaClassParametrs(
                                ScalaClassParametr(name = "", `type` = ScalaImplicitType,
                                    defaultValue = ScalaApplyObject(name = "success",
                                        parametrs = ScalaClassParametrs(
                                            ScalaClassParametr(name = "", `type` = ScalaImplicitType,
                                                defaultValue = ScalaBody(
                                                    ScalaApplyPartialObject(name = "list map",
                                                        body = ScalaCase(
                                                            ScalaCaseLine(expression =
                                                              ScalaExpressionBody(
                                                                  ScalaBody(
                                                                      ScalaApplyObject(name = tupleType,
                                                                          parametrs = boProductObjectParametrs
                                                                      )
                                                                  ),
                                                                  serrializeToOneString = true
                                                              ), caseBody = ScalaBody(s"${productName}(${allColumnsP})")
                                                            )
                                                        )
                                                    )
                                                ), onNewLine = true
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                ),
                ScalaCaseLine(expression = "Failure(x)".expr, caseBody = ScalaBody("ValidationEx(failure(x))")),
                ScalaCaseLine(expression = "x".expr, caseBody = ScalaBody("throw new RuntimeException(s\"Bad branch. (${x})\")"))
            ))

        def selectPOneBody = ScalaBody(
            ScalaCase(expression =
              ScalaExpressionBody(selectOneBody1(suffix = ".result"), serrializeToOneString = true),
                ScalaCaseLine(expression = ScalaExpressionBody(
                    ScalaBody(
                        ScalaApplyObject(name = "Success",
                            parametrs = ScalaClassParametrs(
                                ScalaClassParametr(name = "", `type` = ScalaImplicitType,
                                    defaultValue = ScalaBody(
                                        ScalaApplyObject(name = tupleType,
                                            parametrs = boProductObjectParametrs
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
                                ScalaClassParametr(name = "", `type` = ScalaImplicitType,
                                    defaultValue = ScalaApplyObject(name = "success",
                                        parametrs = ScalaClassParametrs(
                                            ScalaClassParametr(name = "", `type` = ScalaImplicitType,
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
                ScalaCaseLine(expression = "Failure(x)".expr, caseBody = ScalaBody("ValidationEx(failure(x))")),
                ScalaCaseLine(expression = "x".expr, caseBody = ScalaBody("throw new RuntimeException(s\"Bad branch. (${x})\")"))
            ))

        def insertBody = ScalaControlStruct(
            name = "transaction(dataSource)",
            body = insertPartOfBody
        )

        def deleteBody = ScalaControlStruct(
            name = "transaction(dataSource)",
            body = deletePartOfBody
        )

        enumClass addMembers(
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
                          ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"From(${tables.head.tableName.tbl})"),
                          ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue =
                            ScalaBody(ScalaCase(
                                expression = "_columns".expr,
                                ScalaCaseLine(expression = "null".expr, caseBody = "allColumns.fields.toSet intersect columns.toSet".body, serrializeToOneString = true),
                                ScalaCaseLine(expression = "_".expr, caseBody = "_columns.fields.toSet intersect columns.toSet".body, serrializeToOneString = true)
                            ))
                          )
                      )
                  )
              )
          ),
          newLine
          )

        enumClass addMembers(
          ScalaComment("Fetch"),
          selectMethod(ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", ScalaGenerics("FT#ReturnType"))))),
          newLine,
          selectPMethod(ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", ScalaGenerics(s"${productName}")))), sb = selectPBody),
          newLine,
          selectOneMethod(ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("FT#ReturnType")))),
          newLine,
          selectPOneMethod(ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric(s"${productName}"))), sb = selectPOneBody),
          ScalaEndComment("Fetch")
          )

        if (!clazz.isAbstract) {
            def enumObjets = s"Seq(${(clazz.enumValues map (enumValue => clazz.stringToSourceValue(enumValue.key)(clazz.group))).mkString(",".space)})"

            enumClass addMembers(
              newLine,
              ScalaComment("Insert"),
              ScalaVariable(name = "values", body = ScalaBody(enumObjets), serrializeToOneString = true),
              newLine,
              ScalaMethod(
                  name = "insert",
                  body = ScalaBody(insertBody),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", "Int")))),
              newLine,
              ScalaMethod(
                  name = "insertWithoutCommit",
                  body = insertPartOfBody1,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "connection", `type` = "Connection".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("List".cls, ScalaGeneric("Int")))),
              ScalaEndComment("Insert"),
              newLine,
              ScalaComment("Delete"),
              ScalaMethod(
                  name = "delete",
                  body = ScalaBody(deleteBody),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", "Int")))),
              newLine,
              ScalaMethod(
                  name = "deleteWithoutCommit",
                  body = deletePartOfBody1,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "connection", `type` = "Connection".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("List".cls, ScalaGeneric("Int")))),
              ScalaEndComment("Delete")
              )
        }

        val module = ScalaModule(
            packageName.pkg,
            clazz.group.pkg,
            "com.simplesys.jdbc.control._".imp,
            "com.simplesys.bonecp.BoneCPDataSource".imp,
            "java.sql.Connection".imp,
            "com.simplesys.jdbc.control.SessionStructures._".imp,
            "com.simplesys.jdbc.control.ValidationEx".imp,
            "com.simplesys.jdbc.control.classBO.{Where, Set}".imp,
            "org.joda.time.{LocalDateTime, DateTime}".imp,
            "com.simplesys.jdbc.control.table.{From, Insert, InnerJoin}".imp,
            "scalaz.{Failure, Success}".imp,
            s"ru.simplesys.defs.bo.${clazz.group}.table._".imp,
            "com.simplesys.jdbc.NotValue".imp,
            "com.simplesys.jdbc._".imp,
            "com.simplesys.SQL._".imp,
            "com.simplesys.common.Strings._".imp,
            "com.simplesys.SQL.Gen.{SQLAlias, SQLAbsTable, SQLTable}".imp,
            "com.simplesys.jdbc.control.SuperTuple1.FieldProduct".imp,
            "com.simplesys.tuple._".imp,
            s"ru.simplesys.defs.bo.refs.${clazz.className}Values._".imp,
            newLine,
            enumObject,
            newLine,
            enumClass
        )

        //module.log

        res <== {
            out =>
                out(genMessageCreating(s"GenEnums, stage: $stage"))
                out(newLine)
                out(newLine)
                out(module.serrialize())
        }
    }

    def createSeq: Seq[File] = {
        //val classes = schema.classes.filter(_.className === "TestDoc") toArray
        //val classes = schema.classes.filter(_.className === "Gds") toArray
        //val classes = schema.enumClasses.filter(_.className === "TestDoc") toArray
        //val classes = schema.enumClasses.filter(_.className === "StkDocType") toArray

        logger info (s"Begin #844.")
        val classes = schema.enumClasses toArray

        Sorting.quickSort(classes)(IEnumClassOrd)
        val res = classes map (genEnum)
        logger info (s"Done #844.")
        res
    }
}
