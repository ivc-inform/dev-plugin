package ru.simplesys.plugins.sourcegen.app

import ru.simplesys.plugins.sourcegen.meta._
import com.simplesys.common.Strings._
import scala.util.Sorting
import scala.Some
import ru.simplesys.meta.types.{DBDataType, DataType}
import sbt.Logger
import com.simplesys.common.equality.SimpleEquality._

object IClassOrd extends Ordering[IClass] {
    def compare(x: IClass, y: IClass): Int = x.className compare y.className
}

object IEnumClassOrd extends Ordering[IEnumClass] {
    def compare(x: IEnumClass, y: IEnumClass): Int = x.className compare y.className
}

object AttrDefOrd extends Ordering[AttrDef[_]] {
    def compare(x: AttrDef[_], y: AttrDef[_]): Int = x.name compare y.name
}

object ForeignKeyConstraintDefOrd extends Ordering[ForeignKeyConstraintDef] {
    def compare(x: ForeignKeyConstraintDef, y: ForeignKeyConstraintDef): Int = x.softNameForCompare compare y.softNameForCompare
}

object ColumnDefOrd extends Ordering[ColumnDef[_]] {
    def compare(x: ColumnDef[_], y: ColumnDef[_]): Int = x.scalaName compare y.scalaName
}

object ITableOrd extends Ordering[ITable] {
    def compare(x: ITable, y: ITable): Int = x.tableName compare y.tableName
}

object IGroupOrd extends Ordering[IGroup] {
    def compare(x: IGroup, y: IGroup): Int = x.name compare y.name
}

object LinkRefToParentClassOrd extends Ordering[LinkRefToParentClass] {
    def compare(x: LinkRefToParentClass, y: LinkRefToParentClass): Int = x.link.objectName compare y.link.objectName
}

trait Log {
    top =>

    val logger: Logger
    val commentLength = 200
    val indentSize = 4

    implicit class lineSeparator(text: String) {
        def ls(indent: Int = 0): String = fill(commentLength, "=", text)
        def ls1(indent: Int = 0): String = fill(commentLength, "-", text)
    }

    val schema: SchemaDef

    def logBo(boNames: Seq[String] = Seq.empty[String]) {
        var i = 0

        val classes = schema.classes.filter {
            c => (boNames.isEmpty || boNames.exists(_ === c.className))
        } toArray

        Sorting.quickSort(classes)(IClassOrd)
        val indent = 0
        classes foreach {
            c => c.log
                i += 1
        }

        logger.info(spaces(indent) + s"List of classes: ${i}".ls(indent))
        classes foreach {
            c => logger.info(spaces(indent) + c.className)
        }
        logger.info(spaces(indent) + s"Number of classes: ${i}".ls(indent))
    }

    //<editor-fold desc="logIClass">
    implicit class IClassImp(`class`: IClass) {
        val schema: SchemaDef = top.schema

        def log(indent: Int) {
            logIClass(`class`, indent)
        }

        def log: Unit = log(0)
    }

    private def logIClass(`class`: IClass, indent: Int) {
        implicit val schema: SchemaDef = top.schema

        logger.info(spaces(indent) + s"Class: ${`class`.className} group: ${`class`.group.toString}".ls(indent))

        val attrs = `class`.attrs.toArray
        Sorting.quickSort(attrs)(AttrDefOrd)

        logger.info(spaces(indent) + "Attrs:")
        attrs foreach {
            _.log(indent + indentSize)
        }

        val discriminators = `class`.discriminatorVals.toArray
        Sorting.quickSort(discriminators)(LinkRefToParentClassOrd)

        logger.info(spaces(indent) + "discriminators:")
        discriminators foreach {
            _.log(indent + indentSize)
        }

        logger.info(spaces(indent) + "pk:")
        `class`.pk.log(indent + indentSize)

        logger.info(spaces(indent) + "fks:")
        `class`.fks foreach (_.log(indent + indentSize))

        logger.info(spaces(indent) + "ucs:")
        `class`.ucs.filter(_.ucType !== PK) foreach (_.log(indent + indentSize))

        val tables = `class`.linkRefsToAllTables.map(_.toTable).toArray
        Sorting.quickSort(tables)(ITableOrd)

        logger.info(spaces(indent) + "ITables:")
        tables foreach {
            _.log(indent + indentSize)
        }

        logger.info(spaces(indent) + s"End Class: ${`class`.className}".ls(indent))
    }
    //</editor-fold>

    //<editor-fold desc="logAttrDef">
    implicit class AttrDefImp(attr: AttrDef[_]) {
        def log(indent: Int) {
            logAttrDef(attr, indent)
        }

        def log: Unit = log(0)
    }

    private def logAttrDef(attr: AttrDef[_], indent: Int) {
        implicit val schema: SchemaDef = top.schema

        logger.info(spaces(indent) + s"name: ${attr.name}, caption: ${attr.caption}, isMandatory: ${attr.isMandatory}, isCalculated: ${attr.isCalculated}, formula: ${
            attr.formula match {
                case Some(x) => x
                case None => None
            }
        }")

        attr.attrType.log(indent)
        val columns = attr.columns.toArray
        Sorting.quickSort(columns)(ColumnDefOrd)

        logger.info(spaces(indent) + "columns:")
        columns.foreach {
            _.log(indent + indentSize)
        }

        logger.info(spaces(indent) + s"End of attr: ${attr.name}".ls1(indent))

    }
    //</editor-fold>

    //<editor-fold desc="logITable">
    implicit class ItableImp(table: ITable) {
        def log(indent: Int) {
            logITable(table, indent)
        }

        def log: Unit = log(0)
    }

    private def logITable(table: ITable, indent: Int) {
        implicit val schema: SchemaDef = top.schema
        logger.info(spaces(indent) + s"tableName: ${table.tableName}, tableDBName: ${table.tableDBName}")

        val columns = table.columns.toArray
        Sorting.quickSort(columns)(ColumnDefOrd)

        logger.info(spaces(indent) + "columns:")
        columns.foreach {
            _.log(indent + indentSize)
        }

        logger.info(spaces(indent) + "pk:")
        table.pk.log(indent + indentSize)

        logger.info(spaces(indent) + "fks:")
        table.fks foreach (_.log(indent + indentSize))

        logger.info(spaces(indent) + "ucs:")
        table.ucs.filter(_.constraintType !== PK) foreach (_.log(indent + indentSize))

        logger.info(spaces(indent) + s"End of table: ${table.tableName}".ls1(indent))
    }
    //</editor-fold>

    //<editor-fold desc="logDataType">
    implicit class DataTypeImp(dataType: DataType[_]) {
        def log(indent: Int) {
            logDataType(dataType, indent)
        }

        def log: Unit = log(0)
    }
    private def logDataType(dataType: DataType[_], indent: Int) {
        implicit val schema: SchemaDef = top.schema

        logger.info(spaces(indent) + s"DataType:")
        dataType.dbDataType.log(indent + indentSize)
    }
    //</editor-fold>

    //<editor-fold desc="logDataType">
    implicit class DBDataTypeImp(dbDataType: DBDataType) {
        def log(indent: Int) {
            logDBDataType(dbDataType, indent)
        }

        def log: Unit = log(0)
    }
    private def logDBDataType(dBDataType: DBDataType, indent: Int) {
        implicit val schema: SchemaDef = top.schema

        logger.info(spaces(indent) + s"DBDataType: ")
        logger.info(spaces(indent + indentSize) + s"appType: ${dBDataType}, sqlDataType: ${dBDataType.sqlDataType}")
    }
    //</editor-fold>

    //<editor-fold desc="ColumnDef">
    implicit class ColumnDefImp(columnDef: ColumnDef[_]) {
        def log(indent: Int) {
            logColumnDef(columnDef, indent)
        }

        def log: Unit = log(0)
    }
    private def logColumnDef(columnDef: ColumnDef[_], indent: Int) {
        implicit val schema: SchemaDef = top.schema

        logger.info(spaces(indent) + s"scalaName: ${columnDef.scalaName}, dbName: ${columnDef.dbName}, isMandatory: ${columnDef.isMandatory}, tableName: ${columnDef.tableRef.toTable.tableName}")
        columnDef.dataType.log(indent + indentSize)
        logger.info(spaces(indent) + s"End of column: ${columnDef.scalaName}".ls1(indent))
    }
    //</editor-fold>

    //<editor-fold desc="LinkRefToClass">
    implicit class LinkRefToClassImp(linkRefToClass: LinkRefToAbstractClass) {
        def log(indent: Int) {
            logLinkRefToClass(linkRefToClass, indent)
        }

        def log: Unit = log(0)
    }
    private def logLinkRefToClass(linkRefToClass: LinkRefToAbstractClass, indent: Int) {
        implicit val schema: SchemaDef = top.schema

        logger.info(spaces(indent) + s"groupName: ${linkRefToClass.groupName}, objectName: ${linkRefToClass.objectName}")
    }
    //</editor-fold>

    //<editor-fold desc="LinkRefToTable">
    implicit class LinkRefToTableImp(linkRefToTable: LinkRefToTable) {
        def log(indent: Int) {
            logLinkRefToTable(linkRefToTable, indent)
        }

        def log: Unit = log(0)
    }
    private def logLinkRefToTable(linkRefToTable: LinkRefToTable, indent: Int) {
        implicit val schema: SchemaDef = top.schema

        logger.info(spaces(indent) + s"groupName: ${linkRefToTable.groupName}, objectName: ${linkRefToTable.objectName}")
    }
    //</editor-fold>

    //<editor-fold desc="LinkRefToParentClass">
    implicit class LinkRefToParentClassImp(linkRefToParentClass: LinkRefToParentClass) {
        def log(indent: Int) {
            logLinkRefToParentClass(linkRefToParentClass, indent)
        }

        def log: Unit = log(0)
    }
    private def logLinkRefToParentClass(linkRefToParentClass: LinkRefToParentClass, indent: Int) {
        implicit val schema: SchemaDef = top.schema

        linkRefToParentClass.link.log(indent)
        logger.info(spaces(indent + indentSize) + s"discriminatorValue: ${linkRefToParentClass.discriminatorValue}, discriminatorAttrName: ${linkRefToParentClass.toParentClass.discriminatorAttrName}")
    }
    //</editor-fold>

    //<editor-fold desc="UniqueConstraintDef">
    implicit class ConstraintDefImp(constraintDef: ConstraintDef) {
        def log(indent: Int) {
            logConstraintDef(constraintDef, indent)
        }

        def log: Unit = log(0)
    }

    private def logConstraintDef(constraintDef: ConstraintDef, indent: Int) {
        implicit val schema: SchemaDef = top.schema

        logger.info(spaces(indent) + "currentOwner:")
        constraintDef.currentOwner.log(indent + indentSize)
        logger.info(spaces(indent) + "masterOwner:")
        constraintDef.masterOwner.log(indent + indentSize)
        logger.info(spaces(indent) + s"givenName: ${
            constraintDef.givenName match {
                case None => ""
                case Some(x) => x
            }
        }")
        logger.info(spaces(indent) + s"attrNames: ${constraintDef.attrNames.mkString(",")}")
    }
    //</editor-fold>

    //TableConstraintDefData
    implicit class TableConstraintDefDataImp(tableConstraintDefData: TableConstraintDefData) {
        def log(indent: Int) {
            logTableConstraintDefData(tableConstraintDefData, indent)
        }

        def log: Unit = log(0)
    }

    private def logTableConstraintDefData(tableConstraintDefData: TableConstraintDefData, indent: Int) {
        implicit val schema: SchemaDef = top.schema

        logger.info(spaces(indent + indentSize) + s"customDBName: ${tableConstraintDefData.customDBName}, customName: ${tableConstraintDefData.customName}, columnNames: (${tableConstraintDefData.columnNames.mkString(",")})")
    }
}