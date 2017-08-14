package ru.simplesys.plugins.sourcegen.app.Gen

import java.io.File
import java.net.URI

import com.simplesys.common.Strings._
import com.simplesys.common._
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.genSources._
import com.simplesys.io._
import com.simplesys.scalaGen._
import ru.simplesys.plugins.sourcegen.app._
import ru.simplesys.plugins.sourcegen.meta._
import sbt.Logger

import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting
import com.simplesys.file.{Path, PathSet}

class GenBOs(val appFilePath: Path,
             val outFilePath: Path,
             val packageName: String,
             val pkgBOName: String,
             val quoted: Boolean,
             val stage: String,
             val logger: Logger) extends GenScala1 with Log {

    val schemaPath: URI = strEmpty.xsdURI

    val sourceBOFiles: PathSet[Path] = appFilePath * "*.xml"
    implicit val schema = SchemaDef(pkgBOName, sourceBOFiles.files)

    def create: File = ????

    private def genBOs(clazz: IClass): Seq[File] = {
        val res = ArrayBuffer.empty[File]

        val attrs: Array[AttrDef[_]] = clazz.attrsWithOutLob.toArray
        Sorting.quickSort(attrs)(AttrDefOrd)

        val className = clazz.className.bo
        res += genBO(attrs, className, "", false, clazz)


        clazz.attrsWithLob.toArray.foreach {
            attr =>
                val attrPk = clazz.strictUCs.filter(_.ucType == PK).flatMap(_.attrNames).map(clazz.attr).toArray
                val _attrs = attrPk ++ Array(attr)
                Sorting.quickSort(_attrs)(AttrDefOrd)
                res += genBO(_attrs, s"${className}${attr.name.capitalize}", attr.name, true, clazz)
        }

        res
    }

    private def genBO(attrs: Array[AttrDef[_]], className: String, attrName: String, forLob: Boolean, clazz: IClass): File = {

        val addImports = ArrayBuffer.empty[ScalaObjectElement]

        implicit val discriminatorSeq = clazz.discriminatorColumnWVals

        val res: File = (outFilePath / clazz.group / (className + ".scala")).createFile(failIfExists = false).toFile

        val boClass = new ScalaClassDeclare {
            scalaClassGen = className.cls
            parametrs = ScalaClassParametrs(
                ScalaClassParametr(name = "alias", `type` = "SQLAlias".tp, parametrType = ParametrVal, defaultValue = strEmpty)
            )
            parametrsImplicit = ScalaClassParametrs(
                ScalaClassParametr(name = "dataSource", `type` = ScalaPoolDataSource, parametrType = ParametrImplicitVal)
            )
            extensibleClass = ScalaClassGenericExtensible(new ScalaBaseClassDeclare {
                scalaClassGen = "ClassBO".cls
                generics = ScalaGeneric(className)
            })
        }

        val productName = s"${clazz.className.capitalize}${attrName.capitalize}"
        val boProductObjectParametrs = ScalaClassParametrs()

        val boObject = new ScalaClassDeclare {
            scalaClassGen = className.cls
            typeScalaClass = TypeScalaObject
        }

        boObject addMembers(
          ScalaMethod(name = "apply",
              parametrsImplicit = ScalaClassParametrs(
                  ScalaClassParametr(name = "dataSource", `type` = ScalaPoolDataSource, parametrType = ParametrImplicit)
              ), serrializeToOneString = true, body = ScalaBody(s"new ${className}(alias = SQLAlias(strEmpty))")
          ),
          ScalaMethod(name = "apply",
              parametrs = ScalaClassParametrs(
                  ScalaClassParametr(name = "alias", `type` = "SQLAlias".tp)
              ),
              parametrsImplicit = ScalaClassParametrs(
                  ScalaClassParametr(name = "dataSource", `type` = ScalaPoolDataSource, parametrType = ParametrImplicit)
              ),
              serrializeToOneString = true, body = ScalaBody(s"new ${className}(alias = alias)")),
          newLine,
          ScalaVariable(name = "objectName", serrializeToOneString = true, body = ScalaBody(clazz.className.dblQuoted)),
          ScalaVariable(name = "groupName", serrializeToOneString = true, body = ScalaBody(clazz.group.dblQuoted))
          )

        boClass addMembers(
          ScalaComment(s"Class: ${clazz.className}, group: ${clazz.group}"),
          newLine,
          ScalaMethod(name = "this()", parametrsImplicit = ScalaClassParametrs(
              ScalaClassParametr(name = "dataSource", `type` = ScalaPoolDataSource, parametrType = ParametrImplicit)
          ), body = "this(SQLAlias(strEmpty))(dataSource)".body, serrializeToOneString = true),
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
          ScalaVariable(name = "quoted", serrializeToOneString = true, body = ScalaBody(quoted.toString))
          )

        //val tables: Seq[ITable] = clazz.linkRefsToAllTables.map(_.toTable)
        val tables: Seq[ITable] = if (!forLob) clazz.linkRefsToAllTables.map(_.toTable) else Seq(schema.tablesMap(LinkRefToTable(clazz.group, clazz.className)))

        var allColumns = strEmpty
        var allColumns1 = strEmpty
        var allColumnsP = strEmpty
        var forTuple = strEmpty
        var columnTypes = strEmpty
        var tupleType = strEmpty
        var i = 0

        boClass addMember ScalaComment("Tables")
        boClass addMember ScalaVariable(name = "tableCount", body = s"${tables.length}".body, serrializeToOneString = true, `override` = OverrideMofificator)
        tables foreach {
            table =>
                val tableVal = s"${table.tableName.tbl}"
                boClass addMember
                  ScalaVariable(name = s"${tableVal}", body = ScalaBody(
                      ScalaApplyObject(
                          name = if (!forLob) s"""new ${tableVal}(alias = alias + "T${i}".als)""" else s"""new ${tableVal}${attrName.capitalize}(alias = alias + "T${i}".als)"""
                      )), serrializeToOneString = true)
                if (tables.length === 1)
                    boClass addMember ScalaVariable(name = "fromTable", body = s"${tableVal}".body, serrializeToOneString = true, `override` = OverrideMofificator)
                i += 1
        }

        boClass addMember ScalaEndComment("Tables")

        i = 0
        boClass addMembers (ScalaComment("Columns for select"))
        attrs foreach {
            attr =>
                i += 1

                val column = attr.singleColumnsRef.toCol
                val table = column.tableRef.toTable

                val tableVal = table.tableName.tbl
                val columnVal = column.scalaName
                val tblColumn = tableVal + "." + (if (attr.isMandatory) column.scalaName else column.scalaName + "Option")

                allColumns += columnVal.space + "~".space
                allColumns1 += columnVal.space + ",".space

                if (!attr.isCalculated && !attr.isDiscriminator)
                    allColumnsP += columnVal + space + "=".space + columnVal + ",".space

                //val _columnType = column.dataType.scalaTypeAsString(clazz.group)

                val _columnType = {
                    val res = column.dataType.scalaTypeAsString(clazz.group, schema)
                    if (res.indexOf(".") !== -1) {
                        if (!addImports.exists(_.toString() === res.imp.toString())) {
                            addImports += res.imp
                            logger debug (s"Bad type: ${res.dblQuoted} tarnsform to ${res.substring(res.lastIndexOf(".") + 1).dblQuoted} and added import ${res}")
                        }
                        res.substring(res.lastIndexOf(".") + 1)
                    } else
                        res
                }

                val columnType = s"${if (attr.isMandatory) _columnType else s"Array[${_columnType}]"}"

                forTuple += columnVal + ":".space + columnType + ",".space
                boProductObjectParametrs += ScalaClassParametr(name = columnVal, `type` = columnType.tp)
                columnTypes += columnType + ",".space

                val typeColumn = s"${if (!column.dataType.isComplexDataType) column.dataType.scalaTypeAsString(clazz.group, schema) else s"Enum${column.dataType.simpleScalaType}"}${if (attr.isMandatory) strEmpty else "Option"}Column"

                boClass addMember
                  ScalaVariable(
                      name = columnVal,
                      body = if (!column.dataType.isComplexDataType) ScalaBody(ScalaApplyObject(
                          name = typeColumn,
                          parametrs = ScalaClassParametrs(
                              ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = column.dbName.dblQuoted),
                              ScalaClassParametr(name = "nameInBo", `type` = ScalaImplicitType, defaultValue = attr.name.dblQuoted),
                              ScalaClassParametr(name = "caption", `type` = ScalaImplicitType, defaultValue = attr.caption.dblQuoted),
                              ScalaClassParametr(name = "tableColumn", `type` = ScalaImplicitType, defaultValue = tblColumn)
                          )))
                      else
                          ScalaBody(
                              new ScalaClassDeclare {
                                  scalaClassGen = typeColumn.cls
                                  typeScalaClass = AnonimousScalaClass
                                  generics = ScalaGenerics(s"${_columnType}")
                                  parametrs = ScalaClassParametrs(
                                      ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = column.dbName.dblQuoted),
                                      ScalaClassParametr(name = "nameInBo", `type` = ScalaImplicitType, defaultValue = attr.name.dblQuoted),
                                      ScalaClassParametr(name = "caption", `type` = ScalaImplicitType, defaultValue = attr.caption.dblQuoted),
                                      ScalaClassParametr(name = "tableColumn", `type` = ScalaImplicitType, defaultValue = tblColumn)
                                  )
                                  members = if (attr.isMandatory) ArrayBuffer(
                                      ScalaMethod(name = "default", body = ScalaBody(s"${_columnType}.default"), serrializeToOneString = true)
                                  )
                                  else
                                      ArrayBuffer.empty[ScalaElement]
                              }
                          ),
                      serrializeToOneString = !column.dataType.isComplexDataType)
        }

        tupleType = "TupleSS" + i
        columnTypes = "TupleSS" + i + "[" + columnTypes.delLastChar + "]"
        allColumns = if (i > 1) allColumns.delLastChar else allColumns.trim
        allColumns1 = if (i > 1) allColumns1.delLastChar else allColumns1.trim
        allColumnsP = allColumnsP.delLastChar
        forTuple = if (i > 0) forTuple.delLastChar else forTuple.trim

        boClass addMembers(
          newLine,
          ScalaShortComment(s"For select tuple: (${forTuple})"),
          newLine,
          ScalaAliasType(name = "ColumnTypes", body = ScalaBody(columnTypes)),
          ScalaVariable(name = "allColumns", serrializeToOneString = true, body = allColumns.body),
          ScalaVariable(name = "allColumns1", serrializeToOneString = true, body = ScalaBody(s"Seq($allColumns1)"))
          )

        boClass.getConstraints(clazz = clazz, forLob = forLob)
        boClass addMembers(ScalaEndComment("Columns for select"), newLine)

        boClass addMember ScalaComment("Columns for Insert/Update/Delete")

        var insertPartOfBody = ScalaControlBody(expression = "connection".expr, strEmpty)
        var insertPartOfBody1 = ScalaControlBody()

        var updatePartOfBody = ScalaControlBody(expression = "connection".expr, strEmpty)
        var updatePartOfBody1 = ScalaControlBody()

        var insertPPartOfBody = ScalaControlBody(expression = "connection".expr, strEmpty)
        var insertPPartOfBody1 = ScalaControlBody()

        var updatePPartOfBody = ScalaControlBody(expression = "connection".expr, strEmpty)
        var updatePPartOfBody1 = ScalaControlBody()

        var deletePartOfBody = ScalaControlBody(expression = "connection".expr, strEmpty)
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

                val columns: Array[ColumnDef[_]] = if (!forLob) table.columnsWithOutLob.toArray
                else {
                    val columnPkColumnNames = table.pk.columnNames
                    val columnPk: Array[ColumnDef[_]] = table.columns.filter(column => columnPkColumnNames.contains(column.scalaName)).toArray
                    val attrColumn: ColumnDef[_] = table.columnsMap(attrName)
                    columnPk ++ Array(attrColumn)
                }

                Sorting.quickSort(columns)(ColumnDefOrd)

                boClass addMember ScalaComment(s"Table: ${table.tableName}")
                var allColumns = strEmpty
                var columnTypes = strEmpty
                var seqInsertValues = strEmpty
                var seqUpdateValues = strEmpty
                var seqInsertPValues = strEmpty
                var seqUpdatePValues = strEmpty
                var i = 0
                columns foreach {
                    column =>
                        val columnVal = column.scalaName + tableVal
                        val tblColumn = tableVal + "." + (if (column.isMandatory) column.scalaName else column.scalaName + "Option")

                        val idx = getColumnsIndex(column, table)
                        if (idx > 0) {
                            allColumns += columnVal.space + "~".space

                            seqInsertValues += s"Insert(${columnVal}, value _${idx})".space
                            seqInsertPValues += s"Insert(${columnVal}, value.${column.scalaName})".space
                            val columnType = column.dataType.simpleDataType.scalaTypeAsString(table.group, schema)
                            columnTypes += s"${if (column.isMandatory) columnType else "Array[" + columnType + "]"}" + ",".space
                            i += 1

                            seqUpdateValues += s"Set(${column.scalaName}, value _${idx})".space
                            seqUpdatePValues += s"Set(${column.scalaName}, value.${column.scalaName})".space

                            boClass addMember
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

                val insert = ScalaApplyObject(name = s"${tableVal}.insertWithoutCommit", parametrs = ScalaClassParametrs(
                    ScalaClassParametr(name = "connection", `type` = ScalaImplicitType, defaultValue = "connection"),
                    ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = allColumns),
                    ScalaClassParametr(name = strEmpty, `type` = ScalaImplicitType, onNewLine = true, defaultValue = ScalaControlStruct(
                        name = "values map",
                        body = ScalaControlBody(expression = "value".expr, seqInsertValues)
                    )))
                )

                val insertP = ScalaApplyObject(name = s"${tableVal}.insertWithoutCommit", parametrs = ScalaClassParametrs(
                    ScalaClassParametr(name = "connection", `type` = ScalaImplicitType, defaultValue = "connection"),
                    ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = allColumns),
                    ScalaClassParametr(name = strEmpty, `type` = ScalaImplicitType, onNewLine = true, defaultValue = ScalaControlStruct(
                        name = "values map",
                        body = ScalaControlBody(expression = "value".expr, seqInsertPValues)
                    )))
                )

                insertPartOfBody += insert
                insertPartOfBody1 += insert

                insertPPartOfBody += insertP
                insertPPartOfBody1 += insertP

                val update = ScalaApplyObject(
                    name = "updateWithoutCommit",
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "connection", `type` = ScalaImplicitType, defaultValue = "connection"),
                        ScalaClassParametr(name = "setters", `type` = ScalaImplicitType, defaultValue = seqUpdateValues),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                        ScalaClassParametr(name = "table", `type` = ScalaImplicitType, defaultValue = s"SQLTable(${tableVal}.databaseTablename)")
                    )
                )

                val updateP = ScalaApplyObject(
                    name = "updateWithoutCommit",
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "connection", `type` = ScalaImplicitType, defaultValue = "connection"),
                        ScalaClassParametr(name = "setters", `type` = ScalaImplicitType, defaultValue = seqUpdatePValues),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                        ScalaClassParametr(name = "table", `type` = ScalaImplicitType, defaultValue = s"SQLTable(${tableVal}.databaseTablename)")
                    )
                )

                updatePartOfBody += update
                updatePartOfBody1 += update

                updatePPartOfBody += updateP
                updatePPartOfBody1 += updateP

                if (k < tables.length) {
                    insertPartOfBody += ScalaPlusPlusSymbol
                    insertPartOfBody1 += ScalaPlusPlusSymbol

                    updatePartOfBody += ScalaPlusPlusSymbol
                    updatePartOfBody1 += ScalaPlusPlusSymbol

                    insertPPartOfBody += ScalaPlusPlusSymbol
                    insertPPartOfBody1 += ScalaPlusPlusSymbol

                    updatePPartOfBody += ScalaPlusPlusSymbol
                    updatePPartOfBody1 += ScalaPlusPlusSymbol
                }
                k += 1

                boClass addMember ScalaEndComment(s"Table: ${table.tableName}")
        }

        k = 1
        tables.reverse foreach {
            table =>
                val delete = ScalaApplyObject(
                    name = "deleteWithoutCommit",
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "connection", `type` = ScalaImplicitType, defaultValue = "connection"),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
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

        boClass addMembers(ScalaEndComment("Columns for Insert/Update/Delete"), newLine)

        val discriminators: String = (discriminatorSeq.zipWithIndex map {
            case (discriminator, index) => s"${if (index === 0) "Where" else "And"}(${discriminator.colRef.name} === ${if (discriminator.colRef.toCol.dataType.simpleDataType.scalaTypeAsString(discriminator.colRef.table.group, schema) === "String") discriminator.value.dblQuoted else discriminator.value})"
        }).mkString(space)

        val joins: String = (clazz.columnRelationsLinkAllTables map {
            relation =>
                s"InnerJoin(${relation.columnFrom.table.tableName.tbl})(${relation.columnFrom.name + relation.columnFrom.table.tableName.tbl} === ${relation.columnTo.name + relation.columnTo.table.tableName.tbl})."
        }).mkString(space)

        def getJoins(j: String = "join") = {
            if (joins.isEmpty) s"${j}" else joins.trim + space + s"Join(${j})"
        }

        def selectBody(name: String, suffix: String = strEmpty) =
            ScalaBody(
                ScalaApplyObject(name = name,
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "columns"),
                        ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"From(${tables.head.tableName.tbl})"),
                        ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = getJoins()),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                        ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = if (discriminators.trim.isEmpty) "null" else discriminators.trim),
                        ScalaClassParametr(name = "orderBy", `type` = ScalaImplicitType, defaultValue = "orderBy"),
                        ScalaClassParametr(name = "fetchSize", `type` = ScalaImplicitType, defaultValue = "fetchSize"),
                        ScalaClassParametr(name = "dsRequest", `type` = ScalaImplicitType, defaultValue = "dsRequest")
                    ),
                    suffix = suffix
                )
            )

        def selectBody2(name: String, suffix: String = strEmpty) =
            ScalaBody(
                ScalaApplyObjectWithBody(name = name,
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "columns"),
                        ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"From(${tables.head.tableName.tbl})"),
                        ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = getJoins()),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                        ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = if (discriminators.trim.isEmpty) "null" else discriminators.trim),
                        ScalaClassParametr(name = "orderBy", `type` = ScalaImplicitType, defaultValue = "orderBy"),
                        ScalaClassParametr(name = "fetchSize", `type` = ScalaImplicitType, defaultValue = "fetchSize"),
                        ScalaClassParametr(name = "dsRequest", `type` = ScalaImplicitType, defaultValue = "dsRequest")
                    ),
                    body = ScalaBody(ScalaCase(
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
                    ))
                )
            )

        def selectOneBody(suffix: String = strEmpty) =
            ScalaBody(
                ScalaApplyObject(name = "selectOneRoot",
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "columns"),
                        ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"From(${tables.head.tableName.tbl})"),
                        ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = getJoins()),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                        ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = if (discriminators.trim.isEmpty) "null" else discriminators.trim)
                    ),
                    suffix = suffix
                )
            )

        def selectBody1(name: String, suffix: String = strEmpty) =
            ScalaBody(
                ScalaApplyObject(name = name,
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "allColumns"),
                        ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"From(${tables.head.tableName.tbl})"),
                        ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = getJoins()),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                        ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = if (discriminators.trim.isEmpty) "null" else discriminators.trim),
                        ScalaClassParametr(name = "orderBy", `type` = ScalaImplicitType, defaultValue = "orderBy"),
                        ScalaClassParametr(name = "fetchSize", `type` = ScalaImplicitType, defaultValue = "fetchSize"),
                        ScalaClassParametr(name = "dsRequest", `type` = ScalaImplicitType, defaultValue = "dsRequest")
                    ),
                    suffix = suffix
                )
            )

        def selectOneBody1(suffix: String = strEmpty) =
            ScalaBody(
                ScalaApplyObject(name = "selectOneRoot",
                    parametrs = ScalaClassParametrs(
                        ScalaClassParametr(name = "columns", `type` = ScalaImplicitType, defaultValue = "allColumns"),
                        ScalaClassParametr(name = "from", `type` = ScalaImplicitType, defaultValue = s"From(${tables.head.tableName.tbl})"),
                        ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = getJoins()),
                        ScalaClassParametr(name = "where", `type` = ScalaImplicitType, defaultValue = "where"),
                        ScalaClassParametr(name = "discriminator", `type` = ScalaImplicitType, defaultValue = if (discriminators.trim.isEmpty) "null" else discriminators.trim)
                    ),
                    suffix = suffix
                )
            )

        def selectMethod(name: String, nameBodyMethod: String, returnType: AbstractScalaType) = ScalaMethod(
            name = name,
            generics = ScalaGenerics(ScalaGeneric("FT", ScalaUpperBound(ScalaBaseClassDeclare("Product".cls, ScalaAnyInGeneric("with FieldProduct"))))),
            parametrs = ScalaClassParametrs(
                ScalaClassParametr(name = "columns", `type` = "FT".tp, defaultValue = "allColumns"),
                ScalaClassParametr(name = "join", `type` = "JoinParam".tp, defaultValue = "null"),
                ScalaClassParametr(name = "where", `type` = "WhereParam".tp, defaultValue = "null"),
                ScalaClassParametr(name = "orderBy", `type` = "OrderByParam".tp, defaultValue = "null"),
                ScalaClassParametr(name = "fetchSize", `type` = ScalaInt, defaultValue = "dataSource.settings.fetchSize"),
                ScalaClassParametr(name = "dsRequest", `type` = "DSRequest".tp, defaultValue = "null")
            ),
            `type` = returnType,
            body = selectBody(nameBodyMethod))

        def selectPMethod1(name: String, nameBodyMethod: String, returnType: AbstractScalaType) = ScalaMethod(
            name = name,
            generics = ScalaGenerics(ScalaGeneric("FT", ScalaUpperBound(ScalaBaseClassDeclare("Product".cls, ScalaAnyInGeneric("with FieldProduct"))))),
            parametrs = ScalaClassParametrs(
                ScalaClassParametr(name = "columns", `type` = "FT".tp, defaultValue = "allColumns"),
                ScalaClassParametr(name = "join", `type` = "JoinParam".tp, defaultValue = "null"),
                ScalaClassParametr(name = "where", `type` = "WhereParam".tp, defaultValue = "null"),
                ScalaClassParametr(name = "orderBy", `type` = "OrderByParam".tp, defaultValue = "null"),
                ScalaClassParametr(name = "fetchSize", `type` = ScalaInt, defaultValue = "dataSource.settings.fetchSize"),
                ScalaClassParametr(name = "dsRequest", `type` = "DSRequest".tp, defaultValue = "null")
            ),
            `type` = returnType,
            body = selectBody2(nameBodyMethod))

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

        def selectPMethod(name: String, returnType: AbstractScalaType, sb: => ScalaBody) =
            ScalaMethod(
                name = name,
                parametrs = ScalaClassParametrs(
                    ScalaClassParametr(name = "join", `type` = "JoinParam".tp, defaultValue = "null"),
                    ScalaClassParametr(name = "where", `type` = "WhereParam".tp, defaultValue = "null"),
                    ScalaClassParametr(name = "orderBy", `type` = "OrderByParam".tp, defaultValue = "null"),
                    ScalaClassParametr(name = "fetchSize", `type` = ScalaInt, defaultValue = "dataSource.settings.fetchSize"),
                    ScalaClassParametr(name = "dsRequest", `type` = "DSRequest".tp, defaultValue = "null")
                ),
                `type` = returnType,
                body = sb
            )

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
                                        ), suffix = suffix
                                    )
                                )
                            )
                        )
                    )
                ),
                ScalaCaseLine(expression = "Failure(x)".expr, caseBody = ScalaBody("ValidationEx(Failure(x))")),
                ScalaCaseLine(expression = "x".expr, caseBody = ScalaBody("throw new RuntimeException(s\"Bad branch. (${x})\")"))
            ))

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

        def insertBody = ScalaControlStruct(
            name = "transaction(dataSource)",
            body = insertPartOfBody
        )

        def insertPBody = ScalaControlStruct(
            name = "transaction(dataSource)",
            body = insertPPartOfBody
        )

        def updateBody = ScalaControlStruct(
            name = "transaction(dataSource)",
            body = updatePartOfBody
        )

        def updatePBody = ScalaControlStruct(
            name = "transaction(dataSource)",
            body = updatePPartOfBody
        )

        def deleteBody = ScalaControlStruct(
            name = "transaction(dataSource)",
            body = deletePartOfBody
        )

        boClass addMember
          ScalaMethod(name = "preparedBO4Wrap",
              parametrs = ScalaClassParametrs(
                  ScalaClassParametr(name = "columns", `type` = "List[BasicClassBOColumn[_]]".tp)
              ),
              `type` = "PreparedResult".tp,
              body = ScalaBody(
                  ScalaApplyObject(
                      name = "prepareSelect",
                      parametrs = ScalaClassParametrs(
                          ScalaClassParametr(name = "join", `type` = ScalaImplicitType, defaultValue = getJoins("_join")),
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
          )

        boClass addMembers(
          ScalaComment("Fetch"),
          selectMethod("selectList", "selectListRoot", ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", ScalaGenerics("FT#ReturnType"))))),
          newLine,
          selectMethod("selectIterator", "selectIteratorRoot", ScalaClassGenericType(ScalaBaseClassDeclare("ValidationExIterator".cls, ScalaGeneric("Iterator", ScalaGenerics("FT#ReturnType"))))),
          newLine,
          selectOneMethod(ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("FT#ReturnType")))),
          ScalaEndComment("Fetch")
          )


        if (!clazz.isAbstract) {
            boClass addMembers(
              newLine,
              ScalaComment("Fetch Product"),
              newLine,
              selectPMethod1("selectPIterator", "selectPIteratorRoot", ScalaClassGenericType(ScalaBaseClassDeclare("ValidationExIterator".cls, ScalaGeneric("Iterator", ScalaGenerics(productName))))),
              newLine,
              selectPMethod(name = "selectPList", ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", ScalaGenerics(productName)))), sb = selectPBody(name = "selectListRoot", suffix = strEmpty)),
              newLine,
              /*selectPMethod(name = "selectPViewList", ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("SeqView", ScalaGenerics(s"${productName}", ScalaGeneric("List", s"${productName}"))))), sb = selectPBody(name = "selectViewListRoot", suffix = ".view")),
              newLine,*/
              selectPOneMethod(ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric(s"${productName}"))), sb = selectPOneBody),
              ScalaEndComment("Fetch Product"),
              newLine,
              ScalaComment("Insert"),
              newLine,
              ScalaMethod(
                  name = "insert",
                  body = ScalaBody(insertBody),
                  parametrs = ScalaClassParametrs(ScalaClassParametr(name = "values", `type` = (columnTypes + "*").tp)),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", "Int")))),
              newLine,
              ScalaMethod(
                  name = "insertP",
                  body = ScalaBody(insertPBody),
                  parametrs = ScalaClassParametrs(ScalaClassParametr(name = "values", `type` = s"${productName}*".tp)),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", "Int")))),
              newLine,
              ScalaMethod(
                  name = "insertWithoutCommit",
                  body = insertPartOfBody1,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "connection", `type` = "Connection".tp),
                      ScalaClassParametr(name = "values", `type` = (columnTypes + "*").tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("List".cls, ScalaGeneric("Int")))),
              newLine,
              ScalaMethod(
                  name = "insertPWithoutCommit",
                  body = insertPPartOfBody1,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "connection", `type` = "Connection".tp),
                      ScalaClassParametr(name = "values", `type` = s"${productName}*".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("List".cls, ScalaGeneric("Int")))),
              ScalaEndComment("Insert"),
              newLine,
              ScalaComment("Update"),
              ScalaMethod(
                  name = "update",
                  body = ScalaBody(updateBody),
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "value", `type` = columnTypes.tp),
                      ScalaClassParametr(name = "where", `type` = "WhereParam".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", "Int")))),
              newLine,
              ScalaMethod(
                  name = "updateP",
                  body = ScalaBody(updatePBody),
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "value", `type` = productName.tp),
                      ScalaClassParametr(name = "where", `type` = "WhereParam".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", "Int")))),
              newLine,
              ScalaMethod(
                  name = "updateWithoutCommit",
                  body = updatePartOfBody1,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "connection", `type` = "Connection".tp),
                      ScalaClassParametr(name = "value", `type` = columnTypes.tp),
                      ScalaClassParametr(name = "where", `type` = "WhereParam".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("List".cls, ScalaGeneric("Int")))),
              newLine,
              ScalaMethod(
                  name = "updatePWithoutCommit",
                  body = updatePPartOfBody1,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "connection", `type` = "Connection".tp),
                      ScalaClassParametr(name = "value", `type` = productName.tp),
                      ScalaClassParametr(name = "where", `type` = "WhereParam".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("List".cls, ScalaGeneric("Int")))),
              ScalaEndComment("Update"),
              newLine,
              ScalaComment("Delete"),
              ScalaMethod(
                  name = "delete",
                  body = ScalaBody(deleteBody),
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "where", `type` = "WhereParam".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", "Int")))),
              newLine,
              ScalaMethod(
                  name = "deleteWithoutCommit",
                  body = deletePartOfBody1,
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "connection", `type` = "Connection".tp),
                      ScalaClassParametr(name = "where", `type` = "WhereParam".tp)
                  ),
                  `type` = ScalaClassGenericType(ScalaBaseClassDeclare("List".cls, ScalaGeneric("Int")))),
              ScalaEndComment("Delete")
              )
        }

        val module = ScalaModule(
            packageName.pkg,
            newLine,
            clazz.group.pkg,
            newLine,
            "com.simplesys.jdbc.control._".imp,
            "com.simplesys.db.pool.PoolDataSource".imp,
            "java.sql.Connection".imp,
            "com.simplesys.jdbc.control.SessionStructures._".imp,
            "com.simplesys.jdbc.control.ValidationEx".imp,
            "com.simplesys.SQL.Gen.{SQLAlias, SQLAbsTable, SQLTable}".imp,
            "com.simplesys.jdbc.control.classBO.{Where, Set}".imp,
            "com.simplesys.jdbc.control.table.From".imp,
            "org.joda.time.{LocalDateTime, DateTime}".imp,
            "com.simplesys.jdbc.control.table.{Insert, InnerJoin}".imp,
            "scalaz.{Failure, Success}".imp,
            ("ru.simplesys.defs.bo." + clazz.group + ".table._").imp,
            "com.simplesys.common.array._".imp,
            "com.simplesys.jdbc.control.SuperTuple1.FieldProduct".imp,
            "com.simplesys.jdbc._".imp,
            "com.simplesys.SQL._".imp,
            "com.simplesys.common.Strings._".imp,
            "collection.SeqView".imp,
            "com.simplesys.json.JsonElement".imp,
            "com.simplesys.jdbc.control.clob._".imp,
            "com.simplesys.tuple._".imp,
            "com.simplesys.jdbc.control.table.JoinsTable._".imp)

        module ++= addImports.toArray

        module ++=(
          newLine,
          boObject,
          newLine,
          boClass)


        //module.log

        res <== {
            out =>
                out(genMessageCreating(s"GenBOs, stage: $stage"))
                out(newLine)
                out(newLine)
                out(module.serrialize())
        }
    }

    def createSeq: Seq[File] = {

        logger info (s"Begin #819.")

        val classes = (schema.simpleClasses ++ schema.hierarchyClasses) toArray

        Sorting.quickSort(classes)(IClassOrd)

        val res = ArrayBuffer.empty[File]

        classes foreach (res ++= genBOs(_))

        logger info (s"Done #819.")
        res
    }
}
