package ru.simplesys.plugins
package sourcegen
package meta

import ru.simplesys.meta.types._


//---------------------------------------------------------------------------------

class RootHierarchyClassDefData(group: Locator,
                                className: String,
                                classCaption: String,
                                isAbstract: Boolean,
                                strictAttrs: Seq[AttrDef[_]],
                                ucs: Seq[UniqueConstraintDef],
                                fks: Seq[ForeignKeyConstraintDef],
                                defaultSettingsData: IDefaultSettings,
                                isAutoTableMapping: Boolean,
                                useTablePrefix: Boolean,
                                val hierarchyMappingType: HierarchyMappingType,
                                val discriminatorAttrName: String) extends HierarchyClassDefData(group, className, classCaption, isAbstract, strictAttrs, ucs, fks, defaultSettingsData, isAutoTableMapping, useTablePrefix) with HierarchyLevelSettingData

//---------------------------------------------------------------------------------

trait RootHierarchyClassDef extends HierarchyClassDef with HierarchyLevelSetting {
  self: RootHierarchyClassDefData =>
}
