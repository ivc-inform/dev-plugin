package ru.simplesys.plugins
package sourcegen
package meta

import ru.simplesys.meta.types._
import com.simplesys.common.equality.SimpleEquality._

//---------------------------------------------------------------------------------

case class LinkToColumnWValue(val colRef: LinkRefCol, val value: String)
case class ColumnsLinks(val columnFrom: LinkRefCol, val columnTo: LinkRefCol)

//---------------------------------------------------------------------------------

trait ColumnDef[T] {
  def tableRef: LinkRefToTable

  def scalaName: String
  def dbName: String
  def dataType: DataType[T]
  def isMandatory: Boolean
  def scalaTypeAsString(implicit currentGroupName: Locator, resolver: SchemaDef): String = dataType.scalaTypeAsStringConditional(isMandatory)(currentGroupName, resolver)
  def jooqTypeDefAsString(implicit currentGroupName: Locator, resolver: SchemaDef): String = if (isMandatory) dataType.jooqTypeDefMandatory else dataType.jooqTypeDefOptional

  def defaultDBTypeAsString = dataType.dbDataType.getDBString(DataTypes.db)
}

case class GeneratedColumnDef[T](val tableRef: LinkRefToTable,
                                 val name: String,
                                 val dataType: DataType[T],
                                 val isMandatory: Boolean
                                )(implicit schemaDef: SchemaDef) extends ColumnDef[T] {
  def dbName: String = (if (schemaDef.useDbPrefix) if (dataType.dbPrefix.toUpperCase === name.toUpperCase) name else (dataType.dbPrefix + name) else name).toUpperCase
  def scalaName: String = name
}

object ColumnDef {
  def apply[T](tableRef: LinkRefToTable, name: String, dataType: DataType[T], isMandatory: Boolean)(implicit schemaDef: SchemaDef): ColumnDef[T] = {
    GeneratedColumnDef(tableRef, name, dataType, isMandatory)
  }

  def apply(tableRef: LinkRefToTable, name: String, attrs: Seq[AttrDef[_]], classesInTable: Set[LinkRefToAbstractClass])(implicit schemaDef: SchemaDef): ColumnDef[_] = {
    //println(attrs.map(_.currentOwner).toSet.toString() + " - " + classesInTable.toString())
    val dataType = attrs.head.attrType
    val isMandatory = attrs.forall(curr => curr.isMandatory) && (attrs.map(_.currentOwner).toSet === classesInTable)
    GeneratedColumnDef(tableRef, name, dataType, isMandatory)
  }
}

//---------------------------------------------------------------------------------


trait TableConstraintDefData {
  def table: LinkRefToTable
  def customDBName: Option[String]
  def customName: Option[String]
  def constraintType: ConstraintType
  def columnNames: Seq[String]
}


trait TableConstraintDef extends ConstraintImpl {
  self: TableConstraintDefData =>

  def selfRefByName: LinkRefToTableConstraintByName
  def selfRefByAttrs: LinkRefToTableConstraintByCols

  def columnNames: Seq[String]
  def columns(implicit resolver: SchemaDef): Seq[ColumnDef[_]]

  def simpleName: String = {customName getOrElse columnNames.map(_.capitalize).mkString("")}.capitalize

  def scalaName: String = constraintType.key + simpleName
  def dbName(implicit resolver: SchemaDef): String = {
    val resultName = {customDBName getOrElse constraintType.key + "_" + table.toTable.tableDBName + "_" + simpleName}

    val hashValue = util.hashing.MurmurHash3.stringHash(resultName.toUpperCase)
    val hashString = if (hashValue >= 0) "0" + hashValue.toString else "1" + Math.abs(hashValue).toString

    {
      if (resultName.length > 30) (resultName).substring(0, 30 - (hashString.length + 1)) + "_" + hashString
      else resultName
    }.toUpperCase
  }
}

case class UniqueTableConstraintDef(val table: LinkRefToTable,
                                    val customName: Option[String],
                                    val columnNames: Seq[String],
                                    val uniqueType: UniqueConstraintType
                                   ) extends TableConstraintDefData with TableConstraintDef {
  val customDBName = None
  def constraintType = uniqueType
  def columns(implicit resolver: SchemaDef) = {
    val tbl = table.toTable
    columnNames.map(tbl.columnsMap(_))
  }

  def selfRefByName: LinkRefToTableUniqueConstraintByName = LinkRefToTableUniqueConstraintByName(table, scalaName)
  def selfRefByAttrs: LinkRefToTableUniqueConstraintByCols = LinkRefToTableUniqueConstraintByCols(table, columnNames.toSet)

}

object UniqueTableConstraintDef {
  def apply(mapping: ConstraintMapping): Option[UniqueTableConstraintDef] = {
    mapping match {
      case ConstraintMapping(clCons: LinkRefToClassUniqueConstraintByName, tblCons: LinkRefToTableUniqueConstraintByCols, implDetails: UniqueTableConstraintImplDetails) =>
        Some(UniqueTableConstraintDef(tblCons.tableRef, None, implDetails.columnNames, implDetails.ucType))
      case _ => None
    }
  }

  def apply(mapping: Seq[ConstraintMapping]): Option[UniqueTableConstraintDef] = {
    mapping.headOption match {
      case Some(ConstraintMapping(clCons: LinkRefToClassUniqueConstraintByName, tblCons: LinkRefToTableUniqueConstraintByCols, implDetails: UniqueTableConstraintImplDetails)) =>
        Some(UniqueTableConstraintDef(tblCons.tableRef, None, implDetails.columnNames, implDetails.ucType))
      case _ => None
    }
  }
}

case class ForeignKeyTableConstraintDef(val table: LinkRefToTable,
                                        val customName: Option[String],
                                        val fkType: ForeignKeyConstraintType,
                                        val refTable: LinkRefToTable,
                                        val columnMapping: Map[String, String]
                                     ) extends TableConstraintDefData with TableConstraintDef {
  val customDBName = None
  def constraintType = fkType
  def columnNames: Seq[String] = columnMapping.keys.toSeq
  def columns(implicit resolver: SchemaDef) = {
    val tbl = table.toTable
    columnNames.map(tbl.columnsMap(_))
  }

  def selfRefByName: LinkRefToTableForeignConstraintByName = LinkRefToTableForeignConstraintByName(table, scalaName)
  def selfRefByAttrs: LinkRefToTableForeignConstraintByCols = LinkRefToTableForeignConstraintByCols(table, columnNames.toSet)

  def columnsLinks: Seq[ColumnsLinks] = columnMapping.map {case (from, to) => ColumnsLinks(LinkRefCol(table, from), LinkRefCol(refTable, to))}.toSeq

}

object ForeignKeyTableConstraintDef {
  def apply(mapping: ConstraintMapping): Option[ForeignKeyTableConstraintDef] = {
    mapping match {
      case ConstraintMapping(clCons: LinkRefToClassForeignConstraintByName, tblCons: LinkRefToTableForeignConstraintByCols, implDetails: ForeignKeyTableConstraintImplDetails) =>
        Some(ForeignKeyTableConstraintDef(tblCons.tableRef, None, implDetails.fkType, implDetails.referencedTableRef, implDetails.columnMapping))
      case _ => None
    }
  }

  def apply(mapping: Seq[ConstraintMapping]): Option[ForeignKeyTableConstraintDef] = {
    mapping.headOption match {
      case Some(ConstraintMapping(clCons: LinkRefToClassForeignConstraintByName, tblCons: LinkRefToTableForeignConstraintByCols, implDetails: ForeignKeyTableConstraintImplDetails)) =>
        Some(ForeignKeyTableConstraintDef(tblCons.tableRef, None, implDetails.fkType, implDetails.referencedTableRef, implDetails.columnMapping))
      case _ => None
    }
  }

}

//---------------------------------------------------------------------------------
