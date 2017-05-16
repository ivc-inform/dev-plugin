package ru.simplesys.plugins
package sourcegen
package meta

import com.simplesys.common.equality.SimpleEquality._
import ru.simplesys.meta.types.{DomainBlob, DomainClob, Locator}

import scala.collection.mutable.ArrayBuffer
import scala.xml.Elem

trait AbstractClassDefData {
    def group: Locator
    def className: String
    def classCaption: String

    val strictAttrs: Seq[AttrDef[_]]
    val strictAttrsMap: Map[String, AttrDef[_]] = strictAttrs.map(x => (x.name, x)).toMap //(collection.breakOut)

    //  def pk: UniqueConstraintDef
    def strictUCs: Seq[UniqueConstraintDef]
    def strictFKs: Seq[ForeignKeyConstraintDef]

    //def weakFKs: Seq[WeakForeignKeyConstraintDef]

    def isAutoTableMapping: Boolean

    def defaultSettingsData: IDefaultSettings
}

trait AbstractClassDef {
    self: AbstractClassDefData =>

    val selfRef: LinkRefToAbstractClass = LinkRefToClassOld(group, className)
    def group: Locator
    def strictAttrs: Seq[AttrDef[_]]
    def strictUCs: Seq[UniqueConstraintDef]
    def strictFKs: Seq[ForeignKeyConstraintDef]
    def className: String

    //---------------------------------------------------------------------------------
    protected def inheritedAttrDef(attrName: String)(resolver: SchemaDef): Option[AttrDef[_]] = None

    protected def includedByFKAttrDef(attrName: String)(resolver: SchemaDef): Option[AttrDef[_]] = strictFKs.filter(_.attrNames.contains(attrName)).headOption.map { foundFK =>
        implicit val localResolver = resolver
        val temp = foundFK.attrMapping(attrName)(resolver)
        val attrMapping = temp._1
        val origAttrDef = temp._2
        val isMandatoryParent = foundFK.resolveUCConstraint.attrs.find(_.name === attrMapping.remoteName).get.isMandatory
        val mandatory = if (foundFK.isMandatory) isMandatoryParent else false
        //val (attrMapping, origAttrDef) = temp
        //val (attrMapping, origAttrDef) = foundFK.attrMapping(attrName)(resolver)
        val localAttr = origAttrDef.getProxy(selfRef, attrMapping, mandatory, false, false)
        localAttr
    }

    private val privateAttr = Memoize2 {
        (attrName: String, resolver: SchemaDef) => {
            (strictAttrsMap.get(attrName) orElse includedByFKAttrDef(attrName)(resolver) orElse inheritedAttrDef(attrName)(resolver)) match {
                case None =>
                    throw new RuntimeException(s"attrName: $attrName in class: $className not found")
                case x => x
            }
        }.get
    }

    def attr(attrName: String)(implicit resolver: SchemaDef): AttrDef[_] = privateAttr(attrName, resolver)

    def attrNames(implicit resolver: SchemaDef): Seq[String] = strictAttrs.map(_.name) ++ strictFKs.flatMap(_.attrNames)

    def attrs(implicit resolver: SchemaDef): Seq[AttrDef[_]] = attrNames.map(attr)
    def attrsWithOutLob(implicit resolver: SchemaDef): Seq[AttrDef[_]] = attrNames.map(attr).filter(attr => attr.attrType != DomainClob && attr.attrType != DomainBlob)
    def attrsWithLob(implicit resolver: SchemaDef): Seq[AttrDef[_]] = attrNames.map(attr).filter(attr => attr.attrType == DomainClob || attr.attrType == DomainBlob)

    protected def ucsOriginal(resolver: SchemaDef): Seq[UniqueConstraintDef] = strictUCs
    protected def fksOriginal(resolver: SchemaDef): Seq[ForeignKeyConstraintDef] = strictFKs

    private val privateUCs = Memoize1(ucsOriginal)
    private val privateFKs = Memoize1(fksOriginal)

    def ucs(implicit resolver: SchemaDef): Seq[UniqueConstraintDef] = privateUCs(resolver)
    def fks(implicit resolver: SchemaDef): Seq[ForeignKeyConstraintDef] = privateFKs(resolver)
    def pk(implicit resolver: SchemaDef): UniqueConstraintDef = ucs.filter(_.ucType === PK).head
    def pkOption(implicit resolver: SchemaDef): Option[UniqueConstraintDef] = ucs.filter(_.ucType === PK).headOption

    def discriminatorVals(implicit resolver: SchemaDef): Seq[LinkRefToParentClass] = Seq()

    def isAbstract(implicit resolver: SchemaDef): Boolean = false

    def defaultSettings(implicit resolver: SchemaDef): IDefaultSettings = defaultSettingsData
    //  def isAttrOverride(attrName: String)(implicit resolver: SchemaDef): Boolean = false

    def findUCByAttrs(rAttrNames: Set[String])(implicit resolver: SchemaDef): Option[UniqueConstraintDef] = ucs.find(_.attrNames.toSet === rAttrNames)

    //---------------------------------------------------------------------------------


    //---------------------------------------------------------------------------------


    def linkRefToSpecificTable(implicit resolver: SchemaDef): LinkRefToTable = LinkRefToTable(group, autoTableName)
    //stub for customMapping
    def linkRefsToAllTables(implicit resolver: SchemaDef): Seq[LinkRefToTable] = Seq(linkRefToSpecificTable)

    def autoAttrColumnMapping(implicit resolver: SchemaDef): Seq[AttrToColumnMapping] = {
        // here stub for custom mapping too
        //strictAttrs.map(x => AttrToColumnMapping(selfRef, x.name, linkRefToSpecificTable, x.autoColumnName))
        {
            strictAttrs ++ strictFKs.flatMap(_.attrs)
        }.map(x => AttrToColumnMapping(x.selfRef, LinkRefCol(linkRefToSpecificTable, x.autoColumnName)))
    }

    def discriminatorColumnWVals(implicit resolver: SchemaDef): Seq[LinkToColumnWValue] = discriminatorVals.map(_.columnWValue)

    def columnsByAttr(attrName: String)(implicit resolver: SchemaDef): Seq[LinkRefCol] = resolver.mappingForClass(selfRef).withFilter(_.attrLink.name === attrName).map(_.columnLink)
    def columnByAttrAndTable(attrName: String, table: LinkRefToTable)(implicit resolver: SchemaDef): LinkRefCol = resolver.mappingForClass(selfRef).filter(x => x.attrLink.name === attrName && x.columnLink.refTo === table).head.columnLink
    def columnByAttrAndTableOption(attrName: String, table: LinkRefToTable)(implicit resolver: SchemaDef): Option[LinkRefCol] = resolver.mappingForClass(selfRef).filter(x => x.attrLink.name === attrName && x.columnLink.refTo === table).headOption.map(_.columnLink)
    def mappingByTable(table: LinkRefToTable)(implicit resolver: SchemaDef): Seq[AttrToColumnMapping] = resolver.mappingForClass(selfRef).filter(_.columnLink.refTo === table)

    def autoTableName(implicit resolver: SchemaDef): String = className

    def setOfAttrsCanRelyToSingleTable(attrNamesSet: Set[String])(implicit resolver: SchemaDef): Option[LinkRefToTable] = {
        // it should be true for simple classes and autogenerated tables, but if custom mapping will present...
        val mappingForSet: Map[LinkRefToTable, Seq[AttrToColumnMapping]] = resolver.mappingForClass(selfRef).filter(m => attrNamesSet.contains(m.attrLink.name)).groupBy(_.columnLink.refTo)
        mappingForSet.filter { case (tblRef, mapping) => attrNamesSet === mapping.map(_.attrLink.name).toSet }.headOption.map { case (tblRef, mapping) => tblRef }
    }

    def columnRelationsLinkAllTables(implicit resolver: SchemaDef): Seq[ColumnsLinks] = Seq()

    def autoLowLevelTableUCs(implicit resolver: SchemaDef): Seq[UniqueTableConstraintDef] = Seq()
    def autoLowLevelTableFKs(implicit resolver: SchemaDef): Seq[ForeignKeyTableConstraintDef] = Seq()

    //---------------------------------------------------------------------------------
    def classFieldsName = className + "Def"
    def classMetaName = className + "Meta"

    def toXml(implicit resolver: SchemaDef): Seq[Elem] = {
        val _attr = (attrNames map attr)

        val allAttrsWithOutLob = _attr.filter(attr => attr.attrType != DomainClob && attr.attrType != DomainBlob)
        val res = ArrayBuffer.empty[Elem]

        res += <class fullClassName={className} name={className} caption={classCaption} group={group} groupCaption={resolver.groups.filter(_.selfRef === group).head.caption} isAbstract={isAbstract.toString} groupPrefix={resolver.groups.filter(_.selfRef === group).head.prefix}>
            <attrs>
                {allAttrsWithOutLob.map(_.toXML)}
            </attrs>
            <constraints>
                {ucs.map(_.toXml)}{fks.map(_.toXml)}
            </constraints>{defaultSettings.toXml}
        </class>

        val pkAttrs = pk.attrNames

        val allAttrsPk: Seq[AttrDef[_]] = _attr.filter(attr => pkAttrs.contains(attr.name))
        val allAttrsWithLob = _attr.filter(attr => attr.attrType == DomainClob || attr.attrType == DomainBlob)

        allAttrsWithLob.foreach {
            attrLob =>
                val attrsWithLob: Seq[AttrDef[_]] = allAttrsPk ++ Seq(attrLob)

                res += <class fullClassName={className} name={className + attrLob.name.capitalize} lobName={attrLob.name} caption={classCaption} group={group} groupCaption={resolver.groups.filter(_.selfRef === group).head.caption} isAbstract={isAbstract.toString} groupPrefix={resolver.groups.filter(_.selfRef === group).head.prefix}>
                    <attrs>
                        {attrsWithLob.map(_.toXML)}
                    </attrs>
                    <constraints>
                        {pk.toXml}
                    </constraints>
                    <defaults></defaults>
                </class>
        }

        res
    }
}
