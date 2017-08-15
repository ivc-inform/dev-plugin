package ru.simplesys.plugins
package sourcegen
package meta

import com.simplesys.common.XMLs._
import ru.simplesys.meta.types.Locator

import scala.xml._
//---------------------------------------------------------------------------------

trait HierarchyLevelSettingData {
    self: HierarchyClassDefData =>

    def hierarchyMappingType: HierarchyMappingType
    def discriminatorAttrName: String
}

//---------------------------------------------------------------------------------

abstract class HierarchyClassDefData(val group: Locator,
                                     val className: String,
                                     val classCaption: String,
                                     val isAbstractData: Boolean,
                                     val strictAttrs: Seq[AttrDef[_]],
                                     val strictUCs: Seq[UniqueConstraintDef],
                                     val strictFKs: Seq[ForeignKeyConstraintDef],
                                     val defaultSettingsData: IDefaultSettings,
                                     val isAutoTableMapping: Boolean,
                                     val useTablePrefix: Boolean) extends AbstractClassDefData {
}

//---------------------------------------------------------------------------------

trait HierarchyClassDef extends AbstractClassDef {
    self: HierarchyClassDefData =>

    override def isAbstract(implicit resolver: SchemaDef): Boolean = isAbstractData || resolver.haveChild(selfRef)
}

//---------------------------------------------------------------------------------

trait HierarchyLevelSetting {
    self: HierarchyLevelSettingData with AbstractClassDef =>

    def hierarchyMappingType: HierarchyMappingType
    // todo подумать про тип ссылки на атрибут (с учетом того, что атрибут-дискриминатор может быть отнаследован из предыдущих уровней иерархии)
    def discriminatorAttr(implicit resolver: SchemaDef): AttrDef[_] = attr(discriminatorAttrName)(resolver)

    def discriminatorAttrName: String
}


//---------------------------------------------------------------------------------

object HierarchyClassDef {
    def apply(group: Locator, x: Node): IHierarchyClass = {
        val className = (x \ "@name").text
        val classCaption = (x \ "@caption").textOption.getOrElse(className)
        //    println(s"${group}.${className}")
        val classIsAbstract = (x \ "@abstract").textOption.map(_.toBoolean).getOrElse(false)
        val isAutoTableMapping = (x \ "@autoTableMapping").textOption.map(_.toBoolean).getOrElse(true)

        val hierarchySetting = (x \ "hierarchySettings").map(h => {
            val discriminatorField = (h \ "@discriminatorField").text
            val hierarchyMapping = HierarchyMappingType((h \ "@hierarchyMapping").text)
            (discriminatorField, hierarchyMapping)
        }).lastOption

        val referenceToParent = (x \ "referenceToParent").map(LinkRefToParentClass(group, _)).lastOption

        val selfRef = LinkRefToClassBasic(group, className)

        val strictAttrs = (x \\ "attr").map(AttrDef(selfRef, _))

        val strictUCs = (x \\ "uc").map(UniqueConstraintDef(selfRef, _))
        val strictFKs = (x \\ "fk").map(ForeignKeyConstraintDef(selfRef, _))
        val defaultSettings = (x \ "defaults").map(DefaultSettings(selfRef, _)).head
        val useTablePrefix = (x \ "@useTablePrefix").text.asBoolean()

        (referenceToParent, hierarchySetting) match {
            case (Some(p), Some((lDiscriminatorField, lHierarchyMapping))) =>
                val selfRefChild = LinkRefToChildClass(group, className)
                val overrideFKs = (x \\ "fkOverride").map(OverrideForeignKeyConstraintDef(selfRefChild, _))
                new ChildHierarchyClassDefData(group, className, classCaption, classIsAbstract, strictAttrs, strictUCs, strictFKs, overrideFKs, defaultSettings, isAutoTableMapping, useTablePrefix, p) with ChildHierarchyClassDef
                  with AbstractClassDefMetaGen with HierarchyClassDefWSettingMetaGen with ChildHierarchyClassDefMetaGen
                  with HierarchyLevelSettingData with HierarchyLevelSetting {
                    override val hierarchyMappingType = lHierarchyMapping
                    override val discriminatorAttrName = lDiscriminatorField
                }

            case (Some(p), None) =>
                val selfRefChild = LinkRefToChildClass(group, className)
                val overrideFKs = (x \\ "fkOverride").map(OverrideForeignKeyConstraintDef(selfRefChild, _))
                new ChildHierarchyClassDefData(group, className, classCaption, classIsAbstract, strictAttrs, strictUCs, strictFKs, overrideFKs, defaultSettings, isAutoTableMapping, useTablePrefix, p) with ChildHierarchyClassDef
                  with AbstractClassDefMetaGen with /*HierarchyClassDefMetaGen with*/ ChildHierarchyClassDefMetaGen

            case (None, Some((lDiscriminatorField, lHierarchyMapping))) => new RootHierarchyClassDefData(group, className, classCaption, classIsAbstract, strictAttrs, strictUCs, strictFKs, defaultSettings, isAutoTableMapping, useTablePrefix, lHierarchyMapping, lDiscriminatorField) with RootHierarchyClassDef
              with AbstractClassDefMetaGen with HierarchyClassDefWSettingMetaGen with RootHierarchyClassDefMetaGen

            case (None, None) => throw new RuntimeException(s"Invalid hierarchy class definition: ${selfRef.toString}")
        }
    }
}
