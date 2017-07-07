package ru.simplesys.plugins
package sourcegen
package meta

import scala.xml._
import ru.simplesys.meta.types._
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.common.XMLs._

//---------------------------------------------------------------------------------

trait SimpleClassEnumProvider extends EnumProvider

//---------------------------------------------------------------------------------

class SimpleClassDefData(val group: Locator,
                         val className: String,
                         val classCaption: String,
                         val strictAttrs: Seq[AttrDef[_]],
                         val strictUCs: Seq[UniqueConstraintDef],
                         val strictFKs: Seq[ForeignKeyConstraintDef],
                         val defaultSettingsData: IDefaultSettings,
                         val isAutoTableMapping: Boolean) extends AbstractClassDefData {
  //val pk: UniqueConstraintDef = ucs.filter(_.ucType === PK).head
}

//---------------------------------------------------------------------------------

trait SimpleClassDef extends AbstractClassDef {
  self: SimpleClassDefData =>
}

//---------------------------------------------------------------------------------

object SimpleClassDef{
  def apply(group: Locator, x: Node): ISimpleClass = {
    val className = (x \ "@name").text
    val selfRef = LinkRefToClassOld(group, className)
    val classCaption = (x \ "@caption").textOption.getOrElse(className)
    val isAutoTableMapping = (x \ "@autoTableMapping").textOption.map(_.toBoolean).getOrElse(true)
    val strictAttrs = (x \\ "attr").map(AttrDef(selfRef, _))

    val ucs = (x \\ "uc").map(UniqueConstraintDef(selfRef, _))
    val fks = (x \\ "fk").map(ForeignKeyConstraintDef(selfRef, _))

    val defaultSettings = (x \ "defaults").map(DefaultSettings(selfRef, _)).head

    new SimpleClassDefData(group, className, classCaption, strictAttrs, ucs, fks, defaultSettings, isAutoTableMapping) with SimpleClassDef with AbstractClassDefMetaGen with SimpleClassDefMetaGen
  }
}

object EnumClassDef {
  def apply(group: Locator, x: Node): IEnumClass = {
    val className = (x \ "@name").text
    val selfRef = LinkRefToClassOld(group, className)
    val classCaption = (x \ "@caption").textOption.getOrElse(className)
    val isAutoTableMapping = (x \ "@autoTableMapping").textOption.map(_.toBoolean).getOrElse(true)

    val lKeyAttrName = (x \ "@keyAttr").text
    val lNameAttrName = (x \ "@nameAttr").text
    val lCaptionAttrName = (x \ "@captionAttr").text
    //val lDescriptionAttrName = (x \ "@descriptionAttr").text
    val lEnumValues = (x \ "value").map(EnumValueClass(lKeyAttrName, lNameAttrName, lCaptionAttrName, _))

    val ucs = (x \\ "uc").map(UniqueConstraintDef(selfRef, _)) ++ Seq(new SimpleUniqueConstraintDefData(selfRef, None, PK, Seq(lKeyAttrName)) with UniqueConstraintDef)
    val fks = (x \\ "fk").map(ForeignKeyConstraintDef(selfRef, _))

    val pkAttrNames = ucs.filter(_.ucType === PK).head.attrNames

    val strictAttrs = (x \\ "attr").map(AttrDef(selfRef, Some(pkAttrNames, className), _))

    val defaultSettings = (x \ "defaults").map(DefaultSettings(selfRef, _)).head

    new {
      val keyMemberName = lKeyAttrName
      val nameMemberName = lNameAttrName
      val members: Map[String, (DataType[_], Boolean)] = strictAttrs.map(x => (x.name, (x.attrType, x.isMandatory))).toMap
      val enumValues = lEnumValues
      val objName = className
    } with SimpleClassDefData(group, className, classCaption, strictAttrs, ucs, fks, defaultSettings, isAutoTableMapping) with SimpleClassDef with EnumProvider with AbstractClassDefMetaGen with SimpleClassDefMetaGen with EnumProviderMetaGen with EnumClassProviderMetaGen
  }
}
