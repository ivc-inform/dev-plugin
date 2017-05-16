package ru.simplesys.plugins
package sourcegen
package meta

import scala.xml._
import ru.simplesys.meta.types.Locator
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.common.XMLs._
//import ru.simplesys.coreutil.SealedEnumRuntime


//---------------------------------------------------------------------------------

trait LinkRef {
  val groupName: Locator
  val objectName: String

  override def toString: String = groupName + "." + objectName
  def toString(relativeTo: Locator): String = if (relativeTo === groupName) objectName else toString
  def toString(relativeTo: LinkRef): String = if (relativeTo.groupName === groupName) objectName else toString
}



trait LinkRefToAbstractClass extends LinkRef {
  def toClass(implicit resolver: SchemaDef): IClass = resolver.resolveClass(this)
  override def equals(other: Any): Boolean = other match {
    case other: LinkRefToAbstractClass => (other canEqual this) && groupName == other.groupName && objectName == other.objectName
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[LinkRefToAbstractClass]

  override def hashCode: Int = toString.hashCode
}


class LinkRefToClassBasic(val groupName: Locator, val objectName: String) extends LinkRefToAbstractClass

object LinkRefToClassBasic {
  def apply(groupName: Locator, objectName: String): LinkRefToClassBasic = new LinkRefToClassBasic(groupName, objectName)
}

object LinkRefToClassOld {
  def apply(groupName: Locator, objectName: String): LinkRefToClassBasic = new LinkRefToClassBasic(groupName, objectName)
  def apply(group: Locator, x: Node): LinkRefToClassBasic = new LinkRefToClassBasic((x \ "@parentGroup").textOption.getOrElse(group), (x \ "@parent").text)
}

class LinkRefToChildClass(val groupName: Locator, val objectName: String) extends LinkRefToAbstractClass {
  override def toClass(implicit resolver: SchemaDef): IChildHierarchyClass = resolver.resolveClass(this)
}

object LinkRefToChildClass {
  def apply(groupName: Locator, objectName: String): LinkRefToChildClass = new LinkRefToChildClass(groupName, objectName)
}


case class LinkRefToTable(groupName: Locator, objectName: String) extends LinkRef {
  def toTable(implicit resolver: SchemaDef) = resolver.resolveTable(this)
}

object LinkRefToTable {
  def apply(group: Locator, x: Node): LinkRefToTable = LinkRefToTable((x \ "@parentGroup").textOption.getOrElse(group), (x \ "@parent").text)
}

case class LinkRefToParentClass(link: LinkRefToAbstractClass, discriminatorValue: String) {
  def toParentClass(implicit resolver: SchemaDef): IBaseHierarchyClass = {
    resolver.resolveClass(this) match {
      case x: IBaseHierarchyClass => x
      case _ => throw new RuntimeException("Invalid parent class!")
    }
  }

  def columnWValue(implicit resolver: SchemaDef): LinkToColumnWValue = LinkToColumnWValue(toParentClass.discriminatorAttr.singleColumnsRef, discriminatorValue)
}

object LinkRefToParentClass {
  def apply(group: Locator, x: Node): LinkRefToParentClass = LinkRefToParentClass(LinkRefToClassOld(group, x), (x \ "@discriminatorValue").text)
}

//---------------------------------------------------------------------------------

trait LinkRefAbstractAttr {
  val refTo: LinkRef
  val name: String

  override def toString: String = refTo.toString + "." + name
}

case class LinkRefAttr(refTo: LinkRefToAbstractClass, name: String) extends LinkRefAbstractAttr {
  def toAttr(implicit resolver: SchemaDef): AttrDef[_] = refTo.toClass.attr(name)
}


case class LinkRefCol(refTo: LinkRefToTable, name: String) extends LinkRefAbstractAttr {
  def toCol(implicit resolver: SchemaDef): ColumnDef[_] = refTo.toTable.columnsMap(name)
  def table(implicit resolver: SchemaDef): TableDef = refTo.toTable
}

//---------------------------------------------------------------------------------

case class BaseClassInHierarchyProperties(hierarchyMappingType: HierarchyMappingType, discriminatorAttrName: String)

object BaseClassInHierarchyProperties {
  def apply(x: Node): BaseClassInHierarchyProperties = {
    val discriminatorField = (x \ "@discriminatorField").text
    val hierarchyMapping = HierarchyMappingType((x \ "@hierarchyMapping").text)
    BaseClassInHierarchyProperties(hierarchyMapping, discriminatorField)
  }
}

//---------------------------------------------------------------------------------

