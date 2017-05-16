package ru.simplesys.plugins
package sourcegen
package meta

import ru.simplesys.meta.types._
import com.simplesys.common.equality.SimpleEquality._

//class TableDefData

trait TableDef {
//  self: TableDefData =>

  def group: Locator

  def tableName: String
  def scalaTableName: String = s"${tableName}JTbl"

  def autoTableDBName(implicit resolver: SchemaDef): String = (resolver.resolveGroup(group).prefix + "_" + tableName).toUpperCase
  def tableDBName(implicit resolver: SchemaDef): String = autoTableDBName
  val selfRef: LinkRefToTable = LinkRefToTable(group, tableName)
  def columns: Seq[ColumnDef[_]]
  def columnsWithOutLob: Seq[ColumnDef[_]] = columns.filter(dt => dt.dataType != DomainClob && dt.dataType != DomainBlob)
  def columnsWithLob: Seq[ColumnDef[_]] = columns.filter(dt => dt.dataType == DomainClob || dt.dataType == DomainBlob)
  lazy val columnsMap: Map[String, ColumnDef[_]] = columns.map(x => (x.scalaName, x)).toMap

  def ucs: Seq[UniqueTableConstraintDef]
  def fks: Seq[ForeignKeyTableConstraintDef]
  def pk = ucs.filter(_.constraintType === PK).head

  def linksToClasses(implicit resolver: SchemaDef): Set[LinkRefToAbstractClass] = resolver.linksClassForTables(selfRef)

  def fkByTableRefOption(tableRef: LinkRefToTable): Option[ForeignKeyTableConstraintDef] = fks.find(_.refTable === tableRef)
  def fkByTableRef(tableRef: LinkRefToTable): ForeignKeyTableConstraintDef = fks.filter(_.refTable === tableRef).head
}


class GeneratedTableDef(val group: Locator,
                        val name: String,
                        val columns: Seq[ColumnDef[_]],
                        val ucs: Seq[UniqueTableConstraintDef],
                        val fks: Seq[ForeignKeyTableConstraintDef]
                       ) extends TableDef {
  def tableName = name
}

object TableDef {
  def apply(tableRef: LinkRefToTable,
            columns: Seq[ColumnDef[_]],
            ucs: Seq[UniqueTableConstraintDef],
            fks: Seq[ForeignKeyTableConstraintDef]): ITable = new GeneratedTableDef(tableRef.groupName, tableRef.objectName, columns, ucs, fks) with TableDefMetaGen with TableDefDBGen
}
