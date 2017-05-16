package ru.simplesys.plugins
package sourcegen
package meta

import com.simplesys.common.equality.SimpleEquality._


//---------------------------------------------------------------------------------

// children are TableConstraintDef and AppConstraintDef
trait ConstraintImpl

trait AppConstraintDef extends ConstraintImpl
case object AppConstraintDefStub extends AppConstraintDef

//---------------------------------------------------------------------------------

// children are LinkRefToTableConstraint and LinkRefToAppConstraint
trait LinkRefToConstraintImpl

class LinkRefToAppConstraint extends LinkRefToConstraintImpl

trait LinkRefToTableConstraint extends LinkRefToConstraintImpl {
  def tableRef: LinkRefToTable
  def compare(c: TableConstraintDef): Boolean
  def constraint(implicit resolver: SchemaDef): TableConstraintDef
}

trait LinkRefToTableConstraintByName {
  self: LinkRefToTableConstraint =>
  def scalaName: String
  def compare(c: TableConstraintDef): Boolean = c.simpleName === scalaName
}

trait LinkRefToTableConstraintByCols {
  self: LinkRefToTableConstraint =>
  def columnNames: Set[String]
  def compare(c: TableConstraintDef): Boolean = c.columnNames.toSet === columnNames
}

trait LinkRefToTableUniqueConstraint extends LinkRefToTableConstraint {
  def constraint(implicit resolver: SchemaDef): UniqueTableConstraintDef = tableRef.toTable.ucs.filter(compare(_)).head
}

trait LinkRefToTableForeignConstraint extends LinkRefToTableConstraint {
  def constraint(implicit resolver: SchemaDef): ForeignKeyTableConstraintDef = tableRef.toTable.fks.filter(compare(_)).head
}


case class LinkRefToTableUniqueConstraintByName(val tableRef: LinkRefToTable,
                                                val scalaName: String
                                               ) extends LinkRefToTableUniqueConstraint with LinkRefToTableConstraintByName

case class LinkRefToTableUniqueConstraintByCols(val tableRef: LinkRefToTable,
                                                val columnNames: Set[String]
                                               ) extends LinkRefToTableUniqueConstraint with LinkRefToTableConstraintByCols


case class LinkRefToTableForeignConstraintByName(val tableRef: LinkRefToTable,
                                                 val scalaName: String
                                                ) extends LinkRefToTableForeignConstraint with LinkRefToTableConstraintByName

case class LinkRefToTableForeignConstraintByCols(val tableRef: LinkRefToTable,
                                                 val columnNames: Set[String]
                                                ) extends LinkRefToTableForeignConstraint with LinkRefToTableConstraintByCols

//---------------------------------------------------------------------------------

trait ConstraintImplShortDetails

class AppConstraintImplShortDetails extends ConstraintImplShortDetails

trait TableConstraintImplShortDetails extends ConstraintImplShortDetails

case class UniqueTableConstraintImplDetails(val ucType: UniqueConstraintType,
                                            val columnNames: Seq[String]) extends TableConstraintImplShortDetails
case class ForeignKeyTableConstraintImplDetails(val fkType: ForeignKeyConstraintType,
                                                val referencedTableRef: LinkRefToTable,
                                                val columnMapping: Map[String, String]) extends TableConstraintImplShortDetails
