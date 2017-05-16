package ru.simplesys.plugins
package sourcegen

package object meta {

  implicit def linkRefToParentClass2LinkRefToClass(lr: LinkRefToParentClass): LinkRefToAbstractClass = lr.link

  type ISchema              = SchemaDef with SchemaDefMetaGen with SchemaDefDBGen
  type IGroup               = GroupDef
  type IClass               = AbstractClassDef with AbstractClassDefMetaGen
  type ISimpleClass         = SimpleClassDef with AbstractClassDefMetaGen with SimpleClassDefMetaGen
  type IEnumClass           = SimpleClassDef with EnumProvider with AbstractClassDefMetaGen with SimpleClassDefMetaGen with EnumProviderMetaGen
  type IHierarchyClass      = HierarchyClassDef with AbstractClassDefMetaGen// with HierarchyClassDefMetaGen
  type IRootHierarchyClass  = RootHierarchyClassDef with HierarchyLevelSetting with AbstractClassDefMetaGen with HierarchyClassDefWSettingMetaGen
  type IBaseHierarchyClass  = HierarchyClassDef with HierarchyLevelSetting with AbstractClassDefMetaGen with HierarchyClassDefWSettingMetaGen
  type IChildHierarchyClass = ChildHierarchyClassDef with AbstractClassDefMetaGen with ChildHierarchyClassDefMetaGen
  type IDefaultSettings     = DefaultSettingsDef

  type ITable               = TableDef with TableDefMetaGen with TableDefDBGen

  val maxArity = 250
}
