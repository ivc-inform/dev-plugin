package ru.simplesys.plugins
package sourcegen
package meta

import scala.xml._
import ru.simplesys.meta.types.Locator
import com.simplesys.common.equality.SimpleEquality._

//---------------------------------------------------------------------------------

class ChildHierarchyClassDefData(group: Locator,
                                 className: String,
                                 classCaption: String,
                                 isAbstractData: Boolean,
                                 strictAttrs: Seq[AttrDef[_]],
                                 strictUCs: Seq[UniqueConstraintDef],
                                 strictFKs: Seq[ForeignKeyConstraintDef],
                                 val overrideFKs: Seq[ForeignKeyConstraintDef],
                                 defaultSettingsData: IDefaultSettings,
                                 isAutoTableMapping: Boolean,
                                 val parentClassLink: LinkRefToParentClass) extends HierarchyClassDefData(group, className, classCaption, isAbstractData, strictAttrs, strictUCs, strictFKs, defaultSettingsData, isAutoTableMapping)

//---------------------------------------------------------------------------------

trait ChildHierarchyClassDef extends HierarchyClassDef {
  self: ChildHierarchyClassDefData =>


  override val selfRef: LinkRefToChildClass = LinkRefToChildClass(group, className)
  val parentClassLink: LinkRefToParentClass
  def parentClassDef(implicit resolver: SchemaDef): IBaseHierarchyClass = parentClassLink.toParentClass

  override protected def inheritedAttrDef(attrName: String)(resolver: SchemaDef): Option[AttrDef[_]] = {
    Some(parentClassLink.link.toClass(resolver).attr(attrName)(resolver).getProxy(selfRef))
  }

  override def attrNames(implicit resolver: SchemaDef): Seq[String] = {
    val localAttrs = strictAttrs.map(_.name) ++ strictFKs.flatMap(_.attrNames)
    val inheritedAttrs = parentClassLink.link.toClass(resolver).attrNames(resolver).filterNot(inherited => localAttrs.exists(_ === inherited))
    inheritedAttrs ++ localAttrs
  }

  override def discriminatorVals(implicit resolver: SchemaDef): Seq[LinkRefToParentClass] = Seq(parentClassLink) ++ parentClassLink.toParentClass.discriminatorVals(resolver)


  private val overridedFKsNames = overrideFKs.map(_.softNameForCompare).toSet
  override protected def ucsOriginal(resolver: SchemaDef): Seq[UniqueConstraintDef] = parentClassLink.link.toClass(resolver).ucs(resolver).map(_.getProxy(selfRef)) ++ strictUCs
  override protected def fksOriginal(resolver: SchemaDef): Seq[ForeignKeyConstraintDef] = parentClassLink.link.toClass(resolver).fks(resolver).filterNot(inh => overridedFKsNames.contains(inh.softNameForCompare)).map(_.getProxy(selfRef)) ++ strictFKs ++ overrideFKs

  //override def ucs(implicit resolver: SchemaDef): Seq[UniqueConstraintDef] = (parentClassLink.link.toClass(resolver).ucs(resolver)) ++ strictUCs
  //override def fks(implicit resolver: SchemaDef): Seq[ForeignKeyConstraintDef] = (parentClassLink.link.toClass(resolver).fks(resolver)) ++ strictFKs

  def dependsOnHierarchy[T](oneTablePerHierarchy: (IBaseHierarchyClass, SchemaDef) => T, tablePerClass: (IBaseHierarchyClass, SchemaDef) => T)(implicit resolver: SchemaDef): T = {
    parentClassDef.hierarchyMappingType match {
      case OneTablePerHierarchy => oneTablePerHierarchy(parentClassDef, resolver)
      case TablePerClass => tablePerClass(parentClassDef, resolver)
    }

  }

  override def linkRefToSpecificTable(implicit resolver: SchemaDef): LinkRefToTable = dependsOnHierarchy(
    (parent, schema) => parent.linkRefToSpecificTable(schema),
    (parent, schema) =>  LinkRefToTable(group, autoTableName(schema))
  )

  override def linkRefsToAllTables(implicit resolver: SchemaDef): Seq[LinkRefToTable] = dependsOnHierarchy(
    (parent, schema) => parent.linkRefsToAllTables(schema),
    (parent, schema) => parent.linkRefsToAllTables(schema) ++ Seq(linkRefToSpecificTable)
  )

  override def columnRelationsLinkAllTables(implicit resolver: SchemaDef): Seq[ColumnsLinks] = dependsOnHierarchy(
    (parent, schema) => parent.columnRelationsLinkAllTables(schema),
    (parent, schema) => parent.columnRelationsLinkAllTables(schema) ++ {
      val parentTbl = parent.linkRefToSpecificTable(schema)
      val thisTbl = linkRefToSpecificTable(schema)
      thisTbl.toTable.fkByTableRef(parentTbl).columnsLinks
    }
  )
  override def autoAttrColumnMapping(implicit resolver: SchemaDef): Seq[AttrToColumnMapping] = {
    // here can be stub for custom mapping too

    val newPKMapping = dependsOnHierarchy(
                          (parent, schema) => Seq(),
                          (parent, schema) => {pk.attrs.map(x => AttrToColumnMapping(x.selfRef, LinkRefCol(linkRefToSpecificTable, x.autoColumnName)))}
                        )
    val parentClassMapping = parentClassDef.autoAttrColumnMapping.map(m => AttrToColumnMapping(attr(m.attrLink.name).selfRef, m.columnLink))

    val currentMapping = super[HierarchyClassDef].autoAttrColumnMapping(resolver).filterNot(m => parentClassMapping.exists(_.attrLink.name === m.attrLink.name))

    newPKMapping ++ parentClassMapping ++ currentMapping
  }

  override def autoTableName(implicit resolver: SchemaDef): String = dependsOnHierarchy(
    (parent, schema) => parent.autoTableName(schema),
    (parent, schema) => className
  )

  override def autoLowLevelTableUCs(implicit resolver: SchemaDef): Seq[UniqueTableConstraintDef] = dependsOnHierarchy(
    (parent, schema) => Seq(),
    (parent, schema) => {
      val tblRef = linkRefToSpecificTable
      Seq(UniqueTableConstraintDef(tblRef, None, pk.attrNames.map(name => columnByAttrAndTable(name, tblRef).name), PK))
    }
  )

  override def autoLowLevelTableFKs(implicit resolver: SchemaDef): Seq[ForeignKeyTableConstraintDef] = dependsOnHierarchy(
    (parent, schema) => Seq(),
    (parent, schema) => {
      val tblRef = linkRefToSpecificTable
      val parentTblRef = parentClassDef.linkRefToSpecificTable
      Seq(ForeignKeyTableConstraintDef(tblRef, None, RefIsParent, parentTblRef, pk.attrNames.map(name => (columnByAttrAndTable(name, tblRef).name, columnByAttrAndTable(name, parentTblRef).name)).toMap))
    }
  )

  override def defaultSettings(implicit resolver: SchemaDef): IDefaultSettings = DefaultSettings(parentClassDef.defaultSettings, this.defaultSettingsData)
}

//---------------------------------------------------------------------------------


//---------------------------------------------------------------------------------