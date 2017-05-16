package ru.simplesys.plugins
package sourcegen
package meta

import ru.simplesys.coreutil.{SealedCaseClassEnum, SealedEnumRuntime}

//---------------------------------------------------------------------------------

trait SealedEnum extends SealedCaseClassEnum[String] {
  override def toString = key
}

object HierarchyMappingType {
  val hierarchyMappingTypes: Map[String, HierarchyMappingType] = SealedEnumRuntime.values[HierarchyMappingType].map(x => (x.toString, x))(collection.breakOut)

  def apply(x: String): HierarchyMappingType = hierarchyMappingTypes(x)
}

//---------------------------------------------------------------------------------

sealed trait HierarchyMappingType extends SealedEnum

case object OneTablePerHierarchy extends HierarchyMappingType {
  override val key = "oneTablePerHierarchy"
}

case object TablePerClass extends HierarchyMappingType {
  override val key = "tablePerClass"
}

//---------------------------------------------------------------------------------

trait ConstraintType extends SealedEnum

//---------------------------------------------------------------------------------

object UniqueConstraintType {
  val constraintTypes: Map[String, UniqueConstraintType] = SealedEnumRuntime.values[UniqueConstraintType].map(x => (x.toString, x))(collection.breakOut)

  def apply(x: String): UniqueConstraintType = constraintTypes(x)
}

sealed trait UniqueConstraintType extends ConstraintType

case object PK extends UniqueConstraintType {
  override val key = "pk"
}

case object Unique extends UniqueConstraintType {
  override val key = "uq"
}

//---------------------------------------------------------------------------------

object ForeignKeyConstraintType {
  val constraintTypes: Map[String, ForeignKeyConstraintType] = Seq(Assoc, RefIsOwner, RefIsOwned, RefIsParent).map(x => (x.fromString, x))(collection.breakOut)

  def apply(x: String): ForeignKeyConstraintType = constraintTypes(x)
}

sealed trait ForeignKeyConstraintType extends ConstraintType {
  def fromString: String
}

case object Assoc extends ForeignKeyConstraintType {
  override val key = "fa"
  def fromString = "assoc"
}

case object RefIsOwner extends ForeignKeyConstraintType {
  override val key = "fd"
  def fromString = "refIsOwned"
}

case object RefIsOwned extends ForeignKeyConstraintType {
  override val key = "fr"
  def fromString = "refIsOwner"
}

case object RefIsParent extends ForeignKeyConstraintType {
  override val key = "fp"
  def fromString = "refIsParent"
}

//---------------------------------------------------------------------------------

object UIFormType {
  val uiFormTypes: Map[String, UIFormType] = SealedEnumRuntime.values[UIFormType].map(x => (x.toString, x))(collection.breakOut)

  def apply(x: String): UIFormType = uiFormTypes(x)
}

sealed trait UIFormType extends SealedEnum

case object SingleListGrid extends UIFormType {
  override val key = "singleListGrid"
}

case object SingleTreeGrid extends UIFormType {
  override val key = "singleTreeGrid"
}

case object MasterTreeDetailedListGrid extends UIFormType {
  override val key = "masterTreeDetailedListGrid"
}
//---------------------------------------------------------------------------------
