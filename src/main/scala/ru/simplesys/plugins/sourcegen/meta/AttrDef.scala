package ru.simplesys.plugins
package sourcegen
package meta

import ru.simplesys.meta.types._
import scala.xml._
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.common.XMLs._

//---------------------------------------------------------------------------------


trait AttrDef[T] {
    def currentOwner: LinkRefToAbstractClass
    def masterOwner: LinkRefToAbstractClass = currentOwner
    def name: String
    def complexName(implicit resolver: SchemaDef) = s"${currentOwner.toClass.group}_${currentOwner.toClass.className}_${name}"
    def caption: String
    def attrType: DataType[T]
    def isMandatory: Boolean
    def isCalculated: Boolean
    def formula: Option[String]
    def isHidden: Boolean
    def isGenBySeq: Boolean

    def isSystem: Boolean = {
        //    import com.simplesys.common.equality.TypeWiseBalancedEquality._
        attrType == Domain.di || attrType == Domain.id || attrType == Domain.ss
    }

    def selfRef: LinkRefAttr = LinkRefAttr(currentOwner, name)

    /*
      def isOverride(implicit resolver: SchemaDef): Boolean = {
        currentOwner.toClass.isAttrOverride(name)
      }
    */

    def autoColumnName: String = name

    def scalaTypeAsString(implicit currentGroupName: Locator, resolver: SchemaDef): String = attrType.scalaTypeAsStringConditional(isMandatory)(currentGroupName, resolver)
    def stringToSourceValue(s: String)(implicit currentGroupName: Locator, resolver: SchemaDef): String
    def scSimpleType: SCSimpleType = attrType.simpleDataType


    def columnsRefs(implicit resolver: SchemaDef): Seq[LinkRefCol] = currentOwner.toClass.columnsByAttr(name)
    def singleColumnsRef(implicit resolver: SchemaDef): LinkRefCol = columnsRefs.head
    def columns(implicit resolver: SchemaDef): Seq[ColumnDef[_]] = columnsRefs.map(_.toCol)
    def column(table: LinkRefToTable)(implicit resolver: SchemaDef): ColumnDef[_] = currentOwner.toClass.columnByAttrAndTable(name, table).toCol
    def columnOption(table: LinkRefToTable)(implicit resolver: SchemaDef): Option[ColumnDef[_]] = currentOwner.toClass.columnByAttrAndTableOption(name, table).map(_.toCol)

    /*  def getProxy(CurrentOwner: LinkRefToAbstractClass,
                   Name: String = this.name,
                   Caption: String = this.caption,
                   IsMandatory: Boolean = this.isMandatory,
                   IsCalculated: Boolean = this.isCalculated,
                   Formula: Option[String] = this.formula): AttrDef[T]

      def getProxy(CurrentOwner: LinkRefToAbstractClass,
                   mappingSetting: AttrMapping,
                   isMandatory: Boolean
                    ): AttrDef[T]*/

    def toXML(implicit resolver: SchemaDef): Elem = {
        val typeColumn = s"${if (!attrType.isComplexDataType) attrType.scalaTypeAsString(currentOwner.groupName, resolver) else s"${attrType.simpleScalaType}"}${if (isMandatory) "" else "Opt"}"
        val discrNames = currentOwner.toClass.discriminatorVals.map(v => v.toParentClass.discriminatorAttrName)

        if (caption.isEmpty)
           <attr name={name} type={attrType.simpleDataType.name} mandatory={isMandatory.toString} getterType={typeColumn} calculated={isCalculated.toString} isDiscriminator={discrNames.exists(_ === name).toString} hidden={(isHidden || (caption.isEmpty && isSystem)).toString} genBySeq={isGenBySeq.toString}/>
        else
           <attr name={name} type={attrType.simpleDataType.name} caption={caption} mandatory={isMandatory.toString} getterType={typeColumn} calculated={isCalculated.toString} isDiscriminator={discrNames.exists(_ === name).toString} hidden={(isHidden || (caption.isEmpty && isSystem)).toString} genBySeq={isGenBySeq.toString}/>
    }

    def getProxy(CurrentOwner: LinkRefToAbstractClass,
                 Name: String = this.name,
                 Caption: String = this.caption,
                 IsMandatory: Boolean = this.isMandatory,
                 IsCalculated: Boolean = this.isCalculated,
                 Formula: Option[String] = this.formula,
                 IsHidden: Boolean = this.isHidden,
                 IsGenBySeq: Boolean = this.isGenBySeq
                  ): AttrDef[T] = new ProxyAttrDef(this,
    CurrentOwner, {
        if (Name === this.name) None else Some(Name)
    }, {
        if (Caption === this.caption) None else Some(Caption)
    }, {
        if (IsMandatory === this.isMandatory) None else Some(IsMandatory)
    }, {
        if (IsCalculated === this.isCalculated) None else Some(IsCalculated)
    }, {
        if (Formula === this.formula) None else Formula
    }, {
        if (IsHidden === this.isHidden) None else Some(isHidden)
    }, {
        if (IsGenBySeq === this.isGenBySeq) None else Some(isGenBySeq)
    })

    def getProxy(CurrentOwner: LinkRefToAbstractClass,
                 mappingSetting: AttrMapping,
                 isMandatory: Boolean,
                 isHidden: Boolean,
                 isGenBySeq: Boolean
                  ): AttrDef[T] = new ProxyAttrDef(this,
        CurrentOwner,
        Some(mappingSetting.localName),
        mappingSetting.localCaption,
        Some(isMandatory),
        mappingSetting.isCalculated,
        mappingSetting.formula,
        Some(isHidden),
        Some(isGenBySeq))

    /*
      def getCopy(currentOwner: LinkRefToAbstractClass = this.currentOwner,
                  name: String = this.name,
                  caption: String = this.caption,
                  isMandatory: Boolean = this.isMandatory,
                  isCalculated: Boolean = this.isCalculated,
                  formula: Option[String] = this.formula): AttrDef[T]
    */
}

//---------------------------------------------------------------------------------

case class SimpleAttrDef[T](currentOwner: LinkRefToAbstractClass,
                            name: String,
                            caption: String,
                            attrType: DataType[T],
                            isMandatory: Boolean,
                            isCalculated: Boolean,
                            formula: Option[String],
                            isHidden: Boolean,
                            isGenBySeq: Boolean
                             ) extends AttrDef[T] {
    //  self: SimpleAttrDefToXML[T] =>
    def stringToSourceValue(s: String)(implicit currentGroupName: Locator, resolver: SchemaDef): String = {
        currentOwner.toClass match {
            case x: EnumProvider => x.stringToSourceValue(s)(currentGroupName)
            case _ => attrType.simpleScalaType.stringToSourceValueConditional(isMandatory, s)
        }
    }

    /*
      def getCopy(CurrentOwner: LinkRefToAbstractClass = this.currentOwner,
                  Name: String = this.name,
                  Caption: String = this.caption,
                  IsMandatory: Boolean = this.isMandatory,
                  IsCalculated: Boolean = this.isCalculated,
                  Formula: Option[String] = this.formula
                 ): SimpleAttrDef[T] = this.copy(currentOwner = CurrentOwner, name = Name, caption = Caption, isMandatory = IsMandatory, isCalculated = IsCalculated, formula = Formula)
    */
}

//---------------------------------------------------------------------------------

case class EnumAttrDef[T](currentOwner: LinkRefToAbstractClass,
                          name: String,
                          caption: String,
                          attrType: ComplexDataType[T],
                          isMandatory: Boolean,
                          isHidden: Boolean,
                          isGenBySeq: Boolean,
                          enumValues: Seq[EnumValue]) extends AttrDef[T] with EnumProvider with EnumProviderMetaGen {
    def isCalculated = false
    def formula = None

    def keyMemberName = EnumAttrDef.keyMemberName
    def nameMemberName = EnumAttrDef.nameMemberName
    def captionMemberName = EnumAttrDef.captionMemberName

    def group = currentOwner.groupName

    def members: Map[String, (DataType[_], Boolean)] = Map(keyMemberName ->(attrType, true), nameMemberName ->(Domain.sCaption, true), captionMemberName ->(Domain.sCaption, true))

    def objName: String = attrType.scalaComplexType //currentOwner.objectName + name.capitalize

    def stringToSourceValue(s: String)(implicit currentGroupName: Locator, resolver: SchemaDef): String = super[EnumProvider].stringToSourceValue(s)(currentGroupName)

    override def toXML(implicit resolver: SchemaDef): Elem = {
        val node: com.simplesys.xml.Elem = super[AttrDef].toXML
        val res = node.appendElement(super[EnumProvider].toXML)
        res
    }


    override def getProxy(CurrentOwner: LinkRefToAbstractClass,
                          Name: String = this.name,
                          Caption: String = this.caption,
                          IsMandatory: Boolean = this.isMandatory,
                          IsCalculated: Boolean = this.isCalculated,
                          Formula: Option[String] = this.formula,
                          IsHidden: Boolean = this.isHidden,
                          IsGenBySeq: Boolean = this.isGenBySeq
                           ): AttrDef[T] = new EnumProxyAttrDef(this,
    CurrentOwner, {
        if (Name === this.name) None else Some(Name)
    }, {
        if (Caption === this.caption) None else Some(Caption)
    }, {
        if (IsMandatory === this.isMandatory) None else Some(IsMandatory)
    }, {
        if (IsCalculated === this.isCalculated) None else Some(IsCalculated)
    }, {
        if (Formula === this.formula) None else Formula
    }, {
        if (IsHidden === this.isHidden) None else Some(IsHidden)
    }, {
        if (IsGenBySeq === this.isGenBySeq) None else Some(IsGenBySeq)
    })

    override def getProxy(CurrentOwner: LinkRefToAbstractClass,
                          mappingSetting: AttrMapping,
                          isMandatory: Boolean,
                          isHidden: Boolean,
                          isGenBySeq: Boolean
                           ): AttrDef[T] = new EnumProxyAttrDef(this,
        CurrentOwner,
        Some(mappingSetting.localName),
        mappingSetting.localCaption,
        Some(isMandatory),
        mappingSetting.isCalculated,
        mappingSetting.formula,
        Some(isHidden),
        Some(isGenBySeq)
    )

    /*
      def getCopy(CurrentOwner: LinkRefToAbstractClass = this.currentOwner,
                  Name: String = this.name,
                  Caption: String = this.caption,
                  IsMandatory: Boolean = this.isMandatory,
                  IsCalculated: Boolean = this.isCalculated,
                  Formula: Option[String] = this.formula

                 ): EnumAttrDef[T] = this.copy(currentOwner = CurrentOwner, name = Name, caption = Caption, isMandatory = IsMandatory)//this.getCopy(CurrentOwner = CurrentOwner, Name = Name, Caption = Caption, IsMandatory = IsMandatory, IsCalculated = this.isCalculated, Formula = this.formula)
    */
}

object EnumAttrDef {
    def keyMemberName = "code"
    def nameMemberName = "name"
    def captionMemberName = "caption"

    // doesn't supported now!
    def descriptionMemberName = "description"
}

//---------------------------------------------------------------------------------

class ProxyAttrDef[T](val proxy: AttrDef[T],
                      val currentOwner: LinkRefToAbstractClass,
                      val proxyName: Option[String],
                      val proxyCaption: Option[String],
                      val proxyIsMandatory: Option[Boolean],
                      val proxyIsCalculated: Option[Boolean],
                      val proxyFormula: Option[String],
                      val proxyIsHidden: Option[Boolean],
                      val proxyIsGenBySeq: Option[Boolean]
                       ) extends AttrDef[T] {
    //  self: AttrDefToXML[T] =>
    override def masterOwner: LinkRefToAbstractClass = proxy.masterOwner
    def name = proxyName getOrElse proxy.name
    def caption = proxyCaption getOrElse proxy.caption
    def attrType = proxy.attrType
    def isMandatory = proxyIsMandatory getOrElse proxy.isMandatory
    def isCalculated = proxyIsCalculated getOrElse proxy.isCalculated
    def formula = proxyFormula orElse proxy.formula
    def isHidden = proxyIsHidden getOrElse proxy.isHidden
    def isGenBySeq = proxyIsGenBySeq getOrElse proxy.isGenBySeq
    def stringToSourceValue(s: String)(implicit currentGroupName: Locator, resolver: SchemaDef): String = proxy.stringToSourceValue(s)(currentGroupName, resolver)

    //  override def toXML(implicit resolver: SchemaDef): Elem = self.toXML(resolver)

    /*  def getCopy(CurrentOwner: LinkRefToAbstractClass = this.currentOwner,
                  Name: String = this.name,
                  Caption: String = this.caption,
                  IsMandatory: Boolean = this.isMandatory,
                  IsCalculated: Boolean = this.isCalculated,
                  Formula: Option[String] = this.formula
                 ): ProxyAttrDef[T] = this.copy(currentOwner = CurrentOwner,
                                             proxyName = {if (proxy.name != Name) Some(Name) else proxyName},
                                             proxyCaption = {if (proxy.caption != Caption) Some(Caption) else proxyCaption},
                                             proxyIsMandatory = {if (proxy.isMandatory != IsMandatory) Some(IsMandatory) else proxyIsMandatory},
                                             proxyIsCalculated = {if (proxy.isCalculated != IsCalculated) Some(IsCalculated) else proxyIsCalculated},
                                             proxyFormula = {if (proxy.formula != Formula) Formula else proxyFormula}
                                            )*/

}

//---------------------------------------------------------------------------------

class EnumProxyAttrDef[T](proxy: AttrDef[T] with EnumProvider,
                          currentOwner: LinkRefToAbstractClass,
                          proxyName: Option[String],
                          proxyCaption: Option[String],
                          proxyIsMandatory: Option[Boolean],
                          proxyIsCalculated: Option[Boolean],
                          proxyFormula: Option[String],
                          proxyIsHidden: Option[Boolean],
                          proxyIsGenBySeq: Option[Boolean]
                           ) extends ProxyAttrDef[T](proxy, currentOwner, proxyName, proxyCaption, proxyIsMandatory, proxyIsCalculated, proxyFormula, proxyIsHidden, proxyIsGenBySeq) with EnumProvider {

    override def group: Locator = proxy.group

    override def nameMemberName: String = proxy.nameMemberName

    override def members: Map[String, (DataType[_], Boolean)] = proxy.members

    override def objName: String = proxy.objName

    override def enumValues: Seq[EnumValue] = proxy.enumValues

    override def keyMemberName: String = proxy.keyMemberName

    override def toXML(implicit resolver: SchemaDef): Elem = {
        val node: com.simplesys.xml.Elem = super[ProxyAttrDef].toXML
        val res = node.appendElement(super[EnumProvider].toXML)
        res
    }

    override def getProxy(CurrentOwner: LinkRefToAbstractClass,
                          Name: String = this.name,
                          Caption: String = this.caption,
                          IsMandatory: Boolean = this.isMandatory,
                          IsCalculated: Boolean = this.isCalculated,
                          Formula: Option[String] = this.formula,
                          IsHidden: Boolean = this.isHidden,
                          IsGenBySeq: Boolean = this.isGenBySeq
                           ): AttrDef[T] = new EnumProxyAttrDef(this,
    CurrentOwner, {
        if (Name === this.name) None else Some(Name)
    }, {
        if (Caption === this.caption) None else Some(Caption)
    }, {
        if (IsMandatory === this.isMandatory) None else Some(IsMandatory)
    }, {
        if (IsCalculated === this.isCalculated) None else Some(IsCalculated)
    }, {
        if (Formula === this.formula) None else Formula
    }, {
        if (IsHidden === this.isHidden) None else Some(IsHidden)
    }, {
        if (IsGenBySeq === this.isGenBySeq) None else Some(IsGenBySeq)
    })

    override def getProxy(CurrentOwner: LinkRefToAbstractClass,
                          mappingSetting: AttrMapping,
                          isMandatory: Boolean,
                          isHidden: Boolean,
                          isGenBySeq: Boolean
                           ): AttrDef[T] = new EnumProxyAttrDef(this,
        CurrentOwner,
        Some(mappingSetting.localName),
        mappingSetting.localCaption,
        Some(isMandatory),
        mappingSetting.isCalculated,
        mappingSetting.formula,
        Some(isHidden),
        Some(isGenBySeq)
    )

    //override def toXML(implicit resolver: SchemaDef): Elem = super[EnumAttrDefToXML].toXML(resolver)

}

//---------------------------------------------------------------------------------


object AttrDef {
    def apply(currentOwner: LinkRefToAbstractClass, x: Node): AttrDef[_] = apply(currentOwner, None, x)

    def apply(currentOwner: LinkRefToAbstractClass, forPKAttrs: Option[(Seq[String], String)], x: Node): AttrDef[_] = {
        val name = (x \ "@name").text
        val caption = (x \ "@caption").textOption.getOrElse(name)
        //val caption = (x \ "@caption").textOption.getOrElse("") //Из-за отсутствия признака visible нулевой caption принимается `типа` `invisible`
        val dataType = DataTypes.typesMap((x \ "@type").text)
        val isMandatory = (x \ "@mandatory").textOption.exists(_.toBoolean)
        val isCalculated = (x \ "@calculated").textOption.map(_.toBoolean).getOrElse(false)
        val isHidden = (x \ "@hidden").textOption.map(_.toBoolean).getOrElse(false)
        val isGenBySeq = (x \ "@genBySeq").textOption.map(_.toBoolean).getOrElse(false)
        val formula = (x \ "@formula").textOption
        val isSimpleEnum = (x \ "@isSimpleEnum").textOption.map(_.toBoolean).getOrElse(false)
        if (isSimpleEnum) {
            val enumValues = (x \ "value").map(SimpleEnumValue(_))
            val newDataType = new ComplexDataType(currentOwner.groupName, None, currentOwner.objectName + name.capitalize, dataType)
            EnumAttrDef(currentOwner, name, caption, newDataType, isMandatory, isHidden, isGenBySeq, enumValues)
        } else {
            forPKAttrs match {
                case Some((pkAttrNames, typeName)) => {
                    if (pkAttrNames.exists(_ === name)) {
                        val newDataType = new ComplexDataType(currentOwner.groupName, None, typeName, dataType)
                        SimpleAttrDef(currentOwner, name, caption, newDataType, isMandatory, isCalculated, formula, isHidden, isGenBySeq)
                    } else SimpleAttrDef(currentOwner, name, caption, dataType, isMandatory, isCalculated, formula, isHidden, isGenBySeq)
                }
                case None =>
                    SimpleAttrDef(currentOwner, name, caption, dataType, isMandatory, isCalculated, formula, isHidden, isGenBySeq)
            }

        }
    }
}

//---------------------------------------------------------------------------------
