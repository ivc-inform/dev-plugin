package ru.simplesys.plugins
package sourcegen
package meta

import sbt.File
import scala.xml.{Elem, XML}
import ru.simplesys.meta.types.{SchemaDefProto, Locator}
import com.simplesys.common.equality.SimpleEquality._
import scala.util.Sorting
import ru.simplesys.plugins.sourcegen.app.{IClassOrd, AttrDefOrd}


//---------------------------------------------------------------------------------
class SchemaDefData(val prefixPath: String,
                    val groups: Seq[IGroup],
                    val hierarchyClasses: Seq[IHierarchyClass],
                    val enumClasses: Seq[IEnumClass],
                    val simpleClasses: Seq[ISimpleClass] //,
                    /*val customTables: Seq[ITable]*/) {
    val classes: Seq[IClass] = hierarchyClasses ++ enumClasses ++ simpleClasses
    val classesMap: Map[LinkRefToAbstractClass, IClass] = classes.map(x => (x.selfRef, x))(collection.breakOut)
    val groupsMap: Map[Locator, IGroup] = groups.map(x => (x.selfRef, x))(collection.breakOut)

}

//---------------------------------------------------------------------------------

trait SchemaDef extends SchemaDefProto {
    self: SchemaDefData =>

    def groups: Seq[IGroup]
    def simpleClasses: Seq[ISimpleClass]
    def enumClasses: Seq[IEnumClass]
    def hierarchyClasses: Seq[IHierarchyClass]
    def classes: Seq[IClass]

    //  def tables: Seq[ITable]

    // custom mapping will be here!
    protected lazy val mappingAttrColumn: Seq[AttrToColumnMapping] = classes.flatMap {
        _.autoAttrColumnMapping(this)
    }
    protected lazy val mappingConstraints: Seq[ConstraintMapping] = classes.flatMap(x => x.ucs(this).map(_.autoMappingToConsImpl(this)) ++ x.fks(this).map(_.autoMappingToConsImpl(this)))
    protected lazy val nonMappedTableUCs: Seq[UniqueTableConstraintDef] = classes.flatMap(_.autoLowLevelTableUCs(this))
    protected lazy val nonMappedTableFKs: Seq[ForeignKeyTableConstraintDef] = classes.flatMap(_.autoLowLevelTableFKs(this))


    // derived from mappingConstraints
    protected lazy val tableUCsMap: Map[LinkRefToTable, Seq[UniqueTableConstraintDef]] = (mappingConstraints.groupBy(_.implConstraint).flatMap {
        case (lnk, mapp) => UniqueTableConstraintDef(mapp)
    }.toSeq ++ nonMappedTableUCs).groupBy(_.table)
    protected lazy val tableFKsMap: Map[LinkRefToTable, Seq[ForeignKeyTableConstraintDef]] = (mappingConstraints.groupBy(_.implConstraint).flatMap {
        case (lnk, mapp) => ForeignKeyTableConstraintDef(mapp)
    }.toSeq ++ nonMappedTableFKs).groupBy(_.table)
    // custom mapping will be here!

    def linksClassForTables(tableRef: LinkRefToTable): Set[LinkRefToAbstractClass] = mappingAttrColumn.filter(_.columnLink.refTo === tableRef).groupBy(_.attrLink.refTo).keySet
    def mappingForClass(cl: LinkRefToAbstractClass): Seq[AttrToColumnMapping] = mappingAttrColumn.filter(_.attrLink.refTo === cl)
    def mappingForClassAndAttr(attrLink: LinkRefAttr): Seq[AttrToColumnMapping] = mappingAttrColumn.filter(_.attrLink === attrLink)


    lazy val tables: Seq[ITable] = {
        val allMappings = mappingAttrColumn
        val tableMapping = allMappings.groupBy(_.columnLink.refTo)
        val result = tableMapping.map {
            case (tblRef, mapping) =>
                val columns = mapping.groupBy(_.columnLink.name).map {
                    case (colName, currentTableMapping) =>
                        ColumnDef(tblRef, colName, currentTableMapping.map(_.attr(this)), linksClassForTables(tblRef))
                }.toSeq
                TableDef(tblRef, columns, tableUCsMap.getOrElse(tblRef, Seq()), tableFKsMap.getOrElse(tblRef, Seq()))
        }.toSeq
        result
    }

    lazy val tablesMap: Map[LinkRefToTable, ITable] = tables.map(x => (x.selfRef, x))(collection.breakOut)

    def resolveClass(cl: LinkRefToAbstractClass) = classesMap(cl)
    def resolveClass(cl: LinkRefToChildClass): IChildHierarchyClass = classesMap(cl) match {
      case c: IChildHierarchyClass => c
      case _ => throw new RuntimeException(s"class ${cl} is not ICHildHierarchyClass!")
    }
//    def resolveClass(cl: LinkRefToClassOld) = classesMap(cl)
    def resolveTable(tl: LinkRefToTable) = tablesMap(tl)
    def resolveGroup(gr: Locator) = groupsMap(gr)

    def haveChild(cl: LinkRefToAbstractClass): Boolean = hierarchyClasses.collect {
        case x: ChildHierarchyClassDef => x.parentClassLink.link
    } exists (_ === cl)

    def toXML(xsdLocation: String = ""): Elem = {
        val schemaLocation = s"http://simpleSys.ru/xml/library/bo ${xsdLocation}/schema.xsd"
        <allClasses xmlns="http://simpleSys.ru/xml/library/bo"
                    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                    xsi:schemaLocation={schemaLocation}>
            {classes.map(_.toXml(this))}
        </allClasses>
    }
}

//---------------------------------------------------------------------------------

object SchemaDef {
    def apply(prefixPath: String, xmlNodes: Array[Elem]): ISchema = {
        val parseResults: Seq[(IGroup, Seq[IHierarchyClass], Seq[IEnumClass], Seq[ISimpleClass])] = {
            for (xmlPiece <- xmlNodes.toSeq) yield {
                (xmlPiece \\ "group").map {
                    grp =>
                        val group = GroupDef(grp)
                        val hierarchyClasses: Seq[IHierarchyClass] = (grp \\ "classInHierarchy").map(HierarchyClassDef(group.selfRef, _))
                        val enumClasses = (grp \\ "classEnum").map(EnumClassDef(group.selfRef, _))
                        val simpleClasses = (grp \\ "classSimple").map(SimpleClassDef(group.selfRef, _))
                        (group, hierarchyClasses, enumClasses, simpleClasses)
                }
            }
        }.flatten

        val groups = parseResults.map {
            case (group, hierarchyClass, enumClass, simpleClass) => group
        }
        val hierarchyClasses = parseResults.map {
            case (group, hierarchyClass, enumClass, simpleClass) => hierarchyClass
        }.flatten
        val enumClasses = parseResults.map {
            case (group, hierarchyClass, enumClass, simpleClass) => enumClass
        }.flatten
        val simpleClasses = parseResults.map {
            case (group, hierarchyClass, enumClass, simpleClass) => simpleClass
        }.flatten

      val res =  new SchemaDefData(prefixPath, groups, hierarchyClasses, enumClasses, simpleClasses) with SchemaDef with SchemaDefMetaGen with SchemaDefDBGen
//      res.classesMap.keys.foreach(println _)
      res
    }

    def apply(prefixPath: String, files: Seq[File]): SchemaDef with SchemaDefMetaGen with SchemaDefDBGen = {
        val xmlPieces: Array[Elem] = files.map {
            x =>
                //println(x.getAbsolutePath)
                //XML.loadFile(x)
                XML.load(new java.io.InputStreamReader(new java.io.FileInputStream(x), XmlUtil.Encoding))
        }(collection.breakOut)
        apply(prefixPath, xmlPieces)
    }
}
