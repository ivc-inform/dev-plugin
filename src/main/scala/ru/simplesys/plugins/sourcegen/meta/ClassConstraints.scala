package ru.simplesys.plugins
package sourcegen
package meta

import scala.xml._
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.common.XMLs._
import ru.simplesys.meta.types.Locator
import scalaz.syntax.std.option._


//---------------------------------------------------------------------------------


trait LinkRefToClassConstraint {
  def classRef: LinkRefToAbstractClass
  def compare(c: ConstraintDef): Boolean
  def constraint(implicit resolver: SchemaDef): ConstraintDef
}

trait LinkRefToClassConstraintByName {
  self: LinkRefToClassConstraint =>
  def constraintName: String
  def compare(c: ConstraintDef): Boolean = c.softNameForCompare === constraintName
}

trait LinkRefToClassConstraintByAttrs {
  self: LinkRefToClassConstraint =>
  def attrNames: Set[String]
  def compare(c: ConstraintDef): Boolean = c.attrNames.toSet === attrNames
}

trait LinkRefToClassUniqueConstraint extends LinkRefToClassConstraint {
  def constraint(implicit resolver: SchemaDef): UniqueConstraintDef = classRef.toClass.ucs.filter(compare(_)).head
}

trait LinkRefToClassForeignConstraint extends LinkRefToClassConstraint {
  def constraint(implicit resolver: SchemaDef): ForeignKeyConstraintDef = classRef.toClass.fks.filter(compare(_)).head
}


case class LinkRefToClassUniqueConstraintByName(val classRef: LinkRefToAbstractClass,
                                                val constraintName: String
                                                 ) extends LinkRefToClassUniqueConstraint with LinkRefToClassConstraintByName

case class LinkRefToClassUniqueConstraintByAttrs(val classRef: LinkRefToAbstractClass,
                                                val attrNames: Set[String]
                                                 ) extends LinkRefToClassUniqueConstraint with LinkRefToClassConstraintByAttrs


case class LinkRefToClassForeignConstraintByName(val classRef: LinkRefToAbstractClass,
                                                 val constraintName: String
                                                  ) extends LinkRefToClassForeignConstraint with LinkRefToClassConstraintByName

case class LinkRefToClassForeignConstraintByAttrs(val classRef: LinkRefToAbstractClass,
                                                 val attrNames: Set[String]
                                                  ) extends LinkRefToClassForeignConstraint with LinkRefToClassConstraintByAttrs


//---------------------------------------------------------------------------------

trait ConstraintDefData {
    def givenName: Option[String]
    def currentOwner: LinkRefToAbstractClass
//    def constraintType: SealedEnum
    def attrNames: Seq[String]
}


trait ConstraintDef {
  self: ConstraintDefData =>


  def currentOwner: LinkRefToAbstractClass
  def masterOwner: LinkRefToAbstractClass

  def constraintType(implicit resolver: SchemaDef): SealedEnum

  def givenName: Option[String]

  def selfRefByName: LinkRefToClassConstraintByName
  def selfRefByAttrs: LinkRefToClassConstraintByAttrs

//???
  def softNameForCompare: String = givenName | attrNames.map(_.capitalize).mkString("").capitalize
  def constrAttrName(implicit resolver: SchemaDef): String

  def softName(implicit resolver: SchemaDef): String = constraintType.toString() + softNameForCompare

  def toXml(implicit resolver: SchemaDef): Elem
  def attrNames: Seq[String]
  def attrs(implicit resolver: SchemaDef): Seq[AttrDef[_]] = attrNames.map(currentOwner.toClass.attr)

  def getProxy(currentOwner: LinkRefToAbstractClass): ConstraintDef

  def possibleTableRef(implicit resolver: SchemaDef): Option[LinkRefToTable] = currentOwner.toClass.setOfAttrsCanRelyToSingleTable(attrNames.toSet)
  def isMapPossibleToDBConstraint(implicit resolver: SchemaDef): Boolean = possibleTableRef.isDefined
  def autoMappingToConsImpl(implicit resolver: SchemaDef): ConstraintMapping
}

//---------------------------------------------------------------------------------

trait UniqueConstraintDefData extends ConstraintDefData {
  def ucType: UniqueConstraintType
}


trait UniqueConstraintDef extends ConstraintDef {
  self: UniqueConstraintDefData =>

  def ucType: UniqueConstraintType
  def constraintType(implicit resolver: SchemaDef) = ucType

  def classRefName: String = (ucType match {
    case PK => s"${currentOwner.objectName}PK"
    case Unique => s"${currentOwner.objectName}AK${softNameForCompare}"
  }).capitalize

// temporary
  def attrNameIsAutoIncrement(attrName: String)(implicit resolver: SchemaDef): Boolean = {
    val attrNameRes = attrNames.find(x => x === attrName).get
    ucType match {
      case PK => currentOwner.toClass.attr(attrNameRes).attrType.simpleDataType.autoIncGeneratorFunctionName.isDefined
      case _ => false
    }
  }

  def attrNameGetAutoIncrementGenerator(attrName: String)(implicit resolver: SchemaDef): Option[String] = {
    val attrNameRes = attrNames.find(x => x === attrName).get
    ucType match {
      case PK => currentOwner.toClass.attr(attrNameRes).attrType.simpleDataType.autoIncGeneratorFunctionName
      case _ => None
    }
  }
// temporary

  def classRefNameRelative(group: Locator)(implicit resolver: SchemaDef) = if (group === currentOwner.groupName) classRefName else s"${resolver.prefixPath}.${currentOwner.groupName}.${classRefName}"

  override def constrAttrName(implicit resolver: SchemaDef) = ucType match {
    case PK => "pk"
    case Unique => s"uc${softNameForCompare}"
  }

  def selfRefByName: LinkRefToClassUniqueConstraintByName = LinkRefToClassUniqueConstraintByName(currentOwner, softNameForCompare)
  def selfRefByAttrs: LinkRefToClassUniqueConstraintByAttrs = LinkRefToClassUniqueConstraintByAttrs(currentOwner, attrNames.toSet)



  def autoMappingToConsImpl(implicit resolver: SchemaDef): ConstraintMapping = {
    val tblRefOption = possibleTableRef
    val (consImplRef, consImplDetails) = tblRefOption match {
      case Some(tblRef) => {
        val colNames = attrNames.map(n => currentOwner.toClass.columnByAttrAndTable(n, tblRef)).map(_.name)
        (LinkRefToTableUniqueConstraintByCols(tblRef, colNames.toSet), UniqueTableConstraintImplDetails(ucType, colNames))
      }
      case None => (new LinkRefToAppConstraint, new AppConstraintImplShortDetails)
    }
    ConstraintMapping(selfRefByName, consImplRef, consImplDetails)
  }

  def toXml(implicit resolver: SchemaDef): Elem = {
    ucType match {
      case Unique => <unique name={softName}>
        {attrNames.map(x => <attr>{x}</attr>)}
      </unique>
      case PK => <pk name={softName}>
        {attrNames.map(x => <attr>{x}</attr>)}
      </pk>
    }
  }

  def getProxy(CurrentOwner: LinkRefToAbstractClass): UniqueConstraintDef = new ProxyUniqueConstraintDefData(CurrentOwner, this) with UniqueConstraintDef
}

class SimpleUniqueConstraintDefData(val currentOwner: LinkRefToAbstractClass, val givenName: Option[String], val ucType: UniqueConstraintType, val attrNames: Seq[String]) extends UniqueConstraintDefData {
  def masterOwner: LinkRefToAbstractClass = currentOwner
}


class ProxyUniqueConstraintDefData(val currentOwner: LinkRefToAbstractClass, val proxy: UniqueConstraintDefData with UniqueConstraintDef) extends UniqueConstraintDefData {
  def ucType = proxy.ucType
  def attrNames = proxy.attrNames
  def givenName = proxy.givenName
  def masterOwner = proxy.masterOwner
}



object UniqueConstraintDef {
  def apply(originalOwner: LinkRefToAbstractClass, x: Node): UniqueConstraintDef = {
    val givenName = (x \ "@name").textOption
    val ucType = UniqueConstraintType((x \ "@uniqueType").text)
    val attrNames = (x \ "attrName").map(attr => attr.text)
    new SimpleUniqueConstraintDefData(originalOwner, givenName, ucType, attrNames) with UniqueConstraintDef
  }
}

//---------------------------------------------------------------------------------

case class AttrMapping(localName: String, remoteName: String, localCaption: Option[String], /*isMandatory: Option[Boolean],*/ isCalculated: Option[Boolean] = Some(false), formula: Option[String] = None)


object AttrMapping {
  def apply(x: Node): AttrMapping = {
    val localName = (x \ "@attrNameLocal").text
    val remoteName = (x \ "@attrNameRemote").text
    val localCaption = (x \ "@caption").textOption
//    val isMandatory = (x \ "@mandatory").textOption.map(_.toBoolean)
    val isCalculated: Option[Boolean] = None//(x \ "@calculated").textOption.map(_.toBoolean)
    val formula: Option[String] = None
    AttrMapping(localName, remoteName, localCaption,/* isMandatory, */isCalculated, formula)
  }
}
//---------------------------------------------------------------------------------

trait ForeignKeyConstraintDefData extends ConstraintDefData {
/*  def fkTypeData: ForeignKeyConstraintType*/
  def referencedClassRef: LinkRefToAbstractClass
  def attrNameMapping: Seq[AttrMapping]
  def attrNameMap: Map[String, AttrMapping]
}


class SimpleForeignKeyConstraintDefData(val currentOwner: LinkRefToAbstractClass,
                                  val givenName: Option[String],
                                  val fkTypeData: ForeignKeyConstraintType,
                                  val isMandatoryData: Boolean,
                                  val referencedClassRef: LinkRefToAbstractClass,
                                  val attrNameMapping: Seq[AttrMapping]) extends ForeignKeyConstraintDefData {
  val attrNameMap: Map[String, AttrMapping] = attrNameMapping.map(x => (x.localName, x))(collection.breakOut)
  def attrNames = attrNameMap.keys.toSeq
  def masterOwner: LinkRefToAbstractClass = currentOwner
  //def customDBName: Option[String]
  def isMandatory(implicit resolver: SchemaDef): Boolean = isMandatoryData
  def fkType(implicit resolver: SchemaDef) = fkTypeData
}

class OverrideForeignKeyConstraintDefData(val currentOwner: LinkRefToChildClass,
                                          val givenName: Option[String],
                                          val isForceMandatory: Boolean,
                                          val referencedClassRef: LinkRefToAbstractClass,
                                          val attrNameMapping: Seq[AttrMapping]) extends ForeignKeyConstraintDefData with ForeignKeyConstraintDef {
  val attrNameMap: Map[String, AttrMapping] = attrNameMapping.map(x => (x.localName, x))(collection.breakOut)
  def attrNames = attrNameMap.keys.toSeq
  def masterOwner: LinkRefToAbstractClass = currentOwner

  def fkType(implicit resolver: SchemaDef) = currentOwner.toClass.parentClassLink.toParentClass.fks.filter(_.softNameForCompare === this.softNameForCompare).head.fkType
  def isMandatory(implicit resolver: SchemaDef): Boolean = if (isForceMandatory) true else {
//    println(s"${currentOwner}.${softNameForCompare}")
    currentOwner.toClass.parentClassLink.toParentClass.fks.filter(_.softNameForCompare === this.softNameForCompare).head.isMandatory
  }
  //def customDBName: Option[String]
}



class ProxyForeignKeyConstraintDefData(val currentOwner: LinkRefToAbstractClass,
                                       val proxy: ForeignKeyConstraintDefData with ForeignKeyConstraintDef) extends ForeignKeyConstraintDefData {
  def masterOwner: LinkRefToAbstractClass = proxy.masterOwner
  def givenName = proxy.givenName
  def referencedClassRef = proxy.referencedClassRef
  def attrNameMapping = proxy.attrNameMapping
  def attrNameMap = proxy.attrNameMap
  def attrNames = proxy.attrNames

  def fkType(implicit resolver: SchemaDef) = proxy.fkType
  def isMandatory(implicit resolver: SchemaDef): Boolean = proxy.isMandatory
}

trait ForeignKeyConstraintDef extends ConstraintDef {
  self: ForeignKeyConstraintDefData =>

  def fkType(implicit resolver: SchemaDef): ForeignKeyConstraintType
  def isMandatory(implicit resolver: SchemaDef): Boolean
  def constraintType(implicit resolver: SchemaDef) = fkType
  def attrNames: Seq[String]
  override def constrAttrName(implicit resolver: SchemaDef): String = s"${fkType.toString}${softNameForCompare}"
  def referencedClassRef: LinkRefToAbstractClass
  def attrMapping(localAttrName: String)(implicit resolver: SchemaDef): (AttrMapping, AttrDef[_]) = {
    val foundMapping = attrNameMap(localAttrName)
    (foundMapping, referencedClassRef.toClass.attr(foundMapping.remoteName))
  }

  def attrMappingAttrDefs(implicit resolver: SchemaDef): Seq[(AttrDef[_], AttrDef[_])] = {
    val currCl = currentOwner.toClass
    val refCl = referencedClassRef.toClass
    attrNameMapping.map(m => (currCl.attr(m.localName), refCl.attr(m.remoteName)))
  }

  def possibleReferencedTableRef(implicit resolver: SchemaDef): Option[LinkRefToTable] = referencedClassRef.toClass.setOfAttrsCanRelyToSingleTable(attrNameMapping.map(_.remoteName).toSet)
  override def isMapPossibleToDBConstraint(implicit resolver: SchemaDef): Boolean = possibleTableRef.isDefined && possibleReferencedTableRef.isDefined

  def selfRefByName: LinkRefToClassForeignConstraintByName = LinkRefToClassForeignConstraintByName(currentOwner, softNameForCompare)
  def selfRefByAttrs: LinkRefToClassForeignConstraintByAttrs = LinkRefToClassForeignConstraintByAttrs(currentOwner, attrNames.toSet)

  def resolveUCConstraint(implicit resolver: SchemaDef): UniqueConstraintDef = {
//    println(s"from ${selfRefByName.toString} to ${referencedClassRef.toString}")
    referencedClassRef.toClass.findUCByAttrs(attrNameMapping.map(_.remoteName).toSet).get
  }

  def autoMappingToConsImpl(implicit resolver: SchemaDef): ConstraintMapping = {
    val tblRefOption = possibleTableRef
    val referencedTblRefOption = possibleReferencedTableRef
    val (consImplRef, consImplDetails) = (tblRefOption, referencedTblRefOption) match {
      case (Some(tblRef), Some(referencedTblRef)) =>
        val currentClass = currentOwner.toClass
        val referencedClass = referencedClassRef.toClass
        (LinkRefToTableForeignConstraintByCols(
          tblRef,
          attrNames.map(n => currentOwner.toClass.columnByAttrAndTable(n, tblRef)).map(_.name).toSet),
          ForeignKeyTableConstraintImplDetails(fkType, referencedTblRef, attrNameMapping.map(m => (currentClass.columnByAttrAndTable(m.localName, tblRef).name, referencedClass.columnByAttrAndTable(m.remoteName, referencedTblRef).name)).toMap)
        )
      case _ => (new LinkRefToAppConstraint, new AppConstraintImplShortDetails)
    }
    ConstraintMapping(selfRefByName, consImplRef, consImplDetails)
  }

  def toXml(implicit resolver: SchemaDef): Elem = {
    <fk name={softName} referenceTo={referencedClassRef.objectName} referenceToGroup={referencedClassRef.groupName} mandatory={isMandatory.toString}>
       {attrNameMapping.map(x => <attrRef nameLocal={x.localName} nameRemote={x.remoteName}/>)}
     </fk>
  }

  def getProxy(currentOwner: LinkRefToAbstractClass): ForeignKeyConstraintDef = new ProxyForeignKeyConstraintDefData(currentOwner, this) with ForeignKeyConstraintDef
}


object ForeignKeyConstraintDef {
  def apply(originalOwner: LinkRefToAbstractClass, x: Node): ForeignKeyConstraintDef = {
    val givenName = (x \ "@name").textOption
    val fkType = ForeignKeyConstraintType((x \ "@linkType").text)
//    println(s"${originalOwner}.${fkType}")
    val isMandatoryData = (x \ "@mandatory").text.toBoolean

    val linkRef = LinkRefToClassOld(originalOwner.groupName, x)
    val attrMapping: Seq[AttrMapping] = (x \ "mapping").map(AttrMapping(_))

    new SimpleForeignKeyConstraintDefData(originalOwner, givenName, fkType, isMandatoryData, linkRef, attrMapping) with ForeignKeyConstraintDef
  }
}



object OverrideForeignKeyConstraintDef {
  def apply(originalOwner: LinkRefToChildClass, x: Node): ForeignKeyConstraintDef = {
    val givenName = (x \ "@name").textOption
//    val fkType = ForeignKeyConstraintType((x \ "@linkType").text)

    val linkRef = LinkRefToClassOld(originalOwner.groupName, x)
    val attrMapping: Seq[AttrMapping] = (x \ "mapping").map(AttrMapping(_))

    val forceMandatory = (x \ "@forceMandatory").textOption.map(_.toBoolean).getOrElse(false)

    new OverrideForeignKeyConstraintDefData(originalOwner, givenName, forceMandatory, linkRef, attrMapping) with ForeignKeyConstraintDef
  }
}


//---------------------------------------------------------------------------------

