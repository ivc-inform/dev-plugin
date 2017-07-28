package ru.simplesys.plugins.sourcegen.app.Gen

import java.net.URI

import com.simplesys.common.Strings._
import com.simplesys.common._
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.file.{Path, PathSet}
import com.simplesys.genSources._
import com.simplesys.io._
import com.simplesys.scalaGen._
import ru.simplesys.plugins.sourcegen.app._
import ru.simplesys.plugins.sourcegen.meta.{ColumnDef, ITable, SchemaDef}
import sbt.{File, Logger}

import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting

class GenTables(val appFilePath: Path,
                val outFilePath: Path,
                val packageName: String,
                val pkgBOName: String,
                val quoted: Boolean,
                val logger: Logger) extends GenScala1 with Log {

    val schemaPath: URI = "".xsdURI

    val sourceBOFiles: PathSet[Path] = appFilePath * "*.xml"
    implicit val schema = SchemaDef(pkgBOName, sourceBOFiles.files)

    def create: File = ????

    private def genTables(table: ITable): Seq[File] = {
        val res = ArrayBuffer.empty[File]

        val columns = table.columnsWithOutLob.toArray
        Sorting.quickSort(columns)(ColumnDefOrd)

        val className = table.tableName.tbl
        val className4P = table.tableName.capitalize

        res += genTable(columns, className, className4P, table)

        table.columnsWithLob.toArray.foreach {
            column =>
                val columnPkColumnNames = column.tableRef.toTable.pk.columnNames
                val columnPk: Seq[ColumnDef[_]] = columns.filter(column => columnPkColumnNames.contains(column.scalaName))

                res += genTable(columnPk ++ Seq(column), s"${className}${column.scalaName.capitalize}", s"${className4P}${column.scalaName.capitalize}", table)
        }
        res
    }

    private def genTable(columns: Seq[ColumnDef[_]], className: String, className4P: String, table: ITable): File = {

        val classes = table.linksToClasses map (_.toClass)

        val res: File = (outFilePath / table.group / (className + ".scala")).createFile(failIfExists = false).toFile

        val tableClass = new ScalaClassDeclare {
            scalaClassGen = className.cls
            parametrs = ScalaClassParametrs(
                ScalaClassParametr(name = "alias", `type` = "SQLAlias".tp, parametrType = ParametrVal, defaultValue = strEmpty)
            )
            parametrsImplicit = ScalaClassParametrs(
                ScalaClassParametr(name = "dataSource", `type` = ScalaPoolDataSource, parametrType = ParametrImplicitVal)
            )
            extensibleClass = ScalaClassGenericExtensible(
                new ScalaBaseClassDeclare {
                    scalaClassGen = "Table".cls
                    generics = ScalaGeneric(className)
                })
        }

        val tableObject = new ScalaClassDeclare {
            scalaClassGen = className.cls
            typeScalaClass = TypeScalaObject
        }

        tableObject addMembers(
          ScalaMethod(name = "apply",
              parametrsImplicit = ScalaClassParametrs(
                  ScalaClassParametr(
                      name = "dataSource",
                      `type` = ScalaPoolDataSource,
                      parametrType = ParametrImplicit)
              ), serrializeToOneString = true,
              body = ScalaBody(s"new ${className}(alias = SQLAlias(strEmpty))")
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
          ScalaVariable(name = "objectName", serrializeToOneString = true, body = ScalaBody(table.tableName.dblQuoted)),
          ScalaVariable(name = "groupName", serrializeToOneString = true, body = ScalaBody(table.group.dblQuoted))
        )

        tableClass addMembers(
          newLine,
          ScalaVariable(name = "quoted", serrializeToOneString = true, body = ScalaBody(quoted.toString)),
          newLine,
          ScalaMethod(name = "databaseTablename", body = ScalaBody(table.tableDBName.dblQuoted), serrializeToOneString = true),
          newLine,
          ScalaVariable(name = "sqlDialect", serrializeToOneString = true, body = "dataSource.SQLDialect".body),
          newLine
        )

        var allColumns = ""
        var forTuple = ""
        var columnTypes = ""
        var seqValues = ""
        var seqPValues = ""
        var i = 1

        val columnVals = ArrayBuffer.empty[ScalaVariable]

        columns foreach {
            column =>
                val isMandatory = column.isMandatory

                val columnName = if (isMandatory) column.scalaName else column.scalaName + "Option"

                allColumns += columnName.space + "~".space

                val columnType = column.dataType.simpleDataType.scalaTypeAsString(table.group, schema)
                forTuple += columnName + ":".space + s"${if (isMandatory) columnType else s"Array[${columnType}]"}" + ",".space

                seqValues += s"Insert(${columnName}, value _${i})".space
                seqPValues += s"Insert(${columnName}, value.${column.scalaName})".space
                columnTypes += s"${if (isMandatory) columnType else "Array[" + columnType + "]"}" + ",".space
                i += 1

                val attrs = classes.flatMap(_.attrs).filter(attr => attr.name === column.scalaName && attr.isMandatory === isMandatory)

                columnVals += ScalaVariable(
                    name = columnName,
                    body = ScalaBody(ScalaApplyObject(
                        name = s"${columnType}${if (isMandatory) "" else "Option"}ColumnTable",
                        parametrs = ScalaClassParametrs(
                            ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = column.dbName.dblQuoted)
                        ))),
                    serrializeToOneString = true)

                if (classes.size !== attrs.size) {
                    logger debug (s"On table:${table.tableName} for column: ${column.scalaName} mandatory is volatile.")
                    columnVals += ScalaVariable(
                        name = if (!isMandatory) column.scalaName else column.scalaName + "Option",
                        body = ScalaBody(ScalaApplyObject(
                            name = s"${columnType}${if (!isMandatory) "" else "Option"}ColumnTable",
                            parametrs = ScalaClassParametrs(
                                ScalaClassParametr(name = "name", `type` = ScalaImplicitType, defaultValue = column.dbName.dblQuoted)
                            ))),
                        serrializeToOneString = true)
                }
        }

        tableClass ++= columnVals

        columnTypes = "TupleSS" + (i - 1) + "[" + columnTypes.delLastChar + "]"
        seqValues = seqValues.trim

        allColumns = if (columns.length > 1) allColumns.delLastChar else allColumns.trim
        forTuple = if (columns.length > 1) forTuple.delLastChar else forTuple.trim

        tableClass addMembers(
          newLine,
          ScalaShortComment(s"For select tuple: (${forTuple})"),
          newLine,
          ScalaAliasType(name = "ColumnTypes", body = ScalaBody(columnTypes)),
          ScalaMethod(name = "allColumns", serrializeToOneString = true, body = ScalaBody(allColumns)),
          newLine,
          ScalaMethod(
              name = "insert",
              body = ScalaBody(ScalaApplyObject(name = "insertRoot", parametrs = ScalaClassParametrs(
                  ScalaClassParametr(name = "", `type` = ScalaImplicitType, defaultValue = "allColumns"),
                  ScalaClassParametr(name = "", `type` = ScalaImplicitType, defaultValue = ScalaBody(ScalaControlStruct(name = "values map", body = ScalaControlBody(expression = "value".expr, seqValues))))
              ))),
              parametrs = ScalaClassParametrs(ScalaClassParametr(name = "values", `type` = (columnTypes + "*").tp)),
              serrializeToOneString = true, `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", "Int")))),
          newLine,
          ScalaMethod(name = "batch4Insert",
              body = ScalaBody(ScalaApplyObject(name = "batch4Insert", parametrs = ScalaClassParametrs(
                  ScalaClassParametr(name = "", `type` = ScalaImplicitType, defaultValue = "preparedStatement"),
                  ScalaClassParametr(name = "", `type` = ScalaImplicitType, defaultValue = ScalaBody(ScalaControlStruct(name = "values map", body = ScalaControlBody(expression = "value".expr, seqValues))))
              ))),
              parametrs = ScalaClassParametrs(
                  ScalaClassParametr(name = "preparedStatement", `type` = "PreparedStatement".tp),
                  ScalaClassParametr(name = "values", `type` = (columnTypes + "*").tp)), serrializeToOneString = true, `type` = ScalaUnit),
          newLine,
          ScalaMethod(name = "insertWithoutCommit",
              body = ScalaBody(ScalaApplyObject(name = "insertWithoutCommit", parametrs = ScalaClassParametrs(
                  ScalaClassParametr(name = "", `type` = ScalaImplicitType, defaultValue = "connection"),
                  ScalaClassParametr(name = "", `type` = ScalaImplicitType, defaultValue = "allColumns"),
                  ScalaClassParametr(name = "", `type` = ScalaImplicitType, defaultValue = ScalaBody(ScalaControlStruct(name = "values map", body = ScalaControlBody(expression = "value".expr, seqValues))))
              ))),
              parametrs = ScalaClassParametrs(
                  ScalaClassParametr(name = "connection", `type` = "Connection".tp),
                  ScalaClassParametr(name = "values", `type` = (columnTypes + "*").tp)), serrializeToOneString = true, `type` = ScalaClassGenericType(ScalaBaseClassDeclare("List".cls, ScalaGeneric("Int"))))
        )


        if (table.linksToClasses.filter(_.toClass.isAbstract).toSeq.length == 0)
            tableClass addMembers(
              newLine,
              ScalaComment("P Methods"),
              newLine,
              ScalaMethod(
                  name = "insertP",
                  body = ScalaBody(ScalaApplyObject(name = "insertRoot", parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "", `type` = ScalaImplicitType, defaultValue = "allColumns"),
                      ScalaClassParametr(name = "", `type` = ScalaImplicitType, defaultValue = ScalaBody(ScalaControlStruct(name = "values map", body = ScalaControlBody(expression = "value".expr, seqPValues))))
                  ))),
                  parametrs = ScalaClassParametrs(ScalaClassParametr(name = "values", `type` = (className4P + "*").tp)),
                  serrializeToOneString = true, `type` = ScalaClassGenericType(ScalaBaseClassDeclare("ValidationEx".cls, ScalaGeneric("List", "Int")))),
              newLine,
              ScalaMethod(name = "batch4PInsert",
                  body = ScalaBody(ScalaApplyObject(name = "batch4Insert", parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "", `type` = ScalaImplicitType, defaultValue = "preparedStatement"),
                      ScalaClassParametr(name = "", `type` = ScalaImplicitType, defaultValue = ScalaBody(ScalaControlStruct(name = "values map", body = ScalaControlBody(expression = "value".expr, seqPValues))))
                  ))),
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "preparedStatement", `type` = "PreparedStatement".tp),
                      ScalaClassParametr(name = "values", `type` = (className4P + "*").tp)), serrializeToOneString = true, `type` = ScalaUnit),
              newLine,
              ScalaMethod(name = "insertPWithoutCommit",
                  body = ScalaBody(ScalaApplyObject(name = "insertWithoutCommit", parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "", `type` = ScalaImplicitType, defaultValue = "connection"),
                      ScalaClassParametr(name = "", `type` = ScalaImplicitType, defaultValue = "allColumns"),
                      ScalaClassParametr(name = "", `type` = ScalaImplicitType, defaultValue = ScalaBody(ScalaControlStruct(name = "values map", body = ScalaControlBody(expression = "value".expr, seqPValues))))
                  ))),
                  parametrs = ScalaClassParametrs(
                      ScalaClassParametr(name = "connection", `type` = "Connection".tp),
                      ScalaClassParametr(name = "values", `type` = (className4P + "*").tp)), serrializeToOneString = true, `type` = ScalaClassGenericType(ScalaBaseClassDeclare("List".cls, ScalaGeneric("Int")))),
              newLine,
              ScalaEndComment("P Methods")
            )

        val module = ScalaModule(
            packageName.pkg,
            newLine,
            s"${table.group}.table".pkg,
            newLine,
            "com.simplesys.oracle.pool.PoolDataSource".imp,
            "com.simplesys.jdbc.control.{ValidationEx, Table}".imp,
            "com.simplesys.jdbc.control.table.Insert".imp,
            "org.joda.time.{LocalDateTime, DateTime}".imp,
            "java.sql.{Connection, PreparedStatement}".imp,
            "com.simplesys.sql.SQLDialect".imp,
            "com.simplesys.SQL.Gen.SQLAlias".imp,
            "com.simplesys.common.Strings._".imp,
            s"${table.group}.${className4P}".imp,
            "com.simplesys.jdbc.control.clob._".imp,
            "com.simplesys.common.array._".imp,
            "com.simplesys.tuple._".imp,
            newLine,
            tableObject,
            newLine,
            tableClass
        )

        //module.log

        res <== {
            out =>
                out(genMessageCreating("GenTables"))
                out(newLine)
                out(newLine)
                out(module.serrialize())
        }
    }


    def createSeq: Seq[File] = {
        //val tables = schema.tables .filter(_.tableName === "Contractor") toArray
        //val tables = schema.tables.filter(_.tableName === "Gds") toArray
        //val tables = schema.tables.filter(_.tableName === "TestPaging") toArray

        logger info (s"Begin #764.")

        val tables = schema.tables.toArray

        Sorting.quickSort(tables)(ITableOrd)
        val res = ArrayBuffer.empty[File]

        tables foreach (res ++= genTables(_))

        logger info (s"Done #764.")
        res
    }
}
