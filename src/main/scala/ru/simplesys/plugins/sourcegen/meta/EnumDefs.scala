package ru.simplesys.plugins
package sourcegen
package meta

import ru.simplesys.meta.types.{Locator, ScalaType, DataTypes, DataType}
import scala.xml._
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.common.XMLs._

//---------------------------------------------------------------------------------

trait EnumValue {
  def key: String
  def name: String
  def caption: String

  val keyValue: Map[String, String]
}

case class SimpleEnumValue(key: String, name: String, caption: String, description: Option[String]) extends EnumValue {

  override val keyValue: Map[String, String] = Map(EnumAttrDef.keyMemberName -> key, EnumAttrDef.nameMemberName -> name, EnumAttrDef.captionMemberName -> caption/*, "description" -> description*/)
}

object SimpleEnumValue {
  def apply(x: Node): EnumValue = SimpleEnumValue((x \ s"@${EnumAttrDef.keyMemberName}").text, (x \ s"@${EnumAttrDef.nameMemberName}").text, (x \ s"@${EnumAttrDef.captionMemberName}").text, (x \ s"@${EnumAttrDef.descriptionMemberName}").textOption)
}

case class EnumValueClass(val key: String, val name: String, val caption: String, override val keyValue: Map[String, String]) extends EnumValue

object EnumValueClass {
  def apply(keyAttr: String, nameAttr: String, captionAttr: String, x: Node): EnumValue =
  {
    val values: Map[String, String] = x.attributes.map(x => (x.key, x.value.text))(collection.breakOut)
    EnumValueClass(values(keyAttr), values(nameAttr), values(captionAttr), values)
  }
}

trait EnumProvider {
  def group: Locator
  def keyMemberName: String
  def nameMemberName: String

  def keyMemberType: DataType[_] = members(keyMemberName)._1
  def keyIsMandatory: Boolean = members(keyMemberName)._2

  def members: Map[String, (DataType[_], Boolean)]

  def enumValues: Seq[EnumValue]

  def objName: String
  def objValuesName: String = s"${objName}Values"

  def objJOOQMapperName: String = s"To${objName}"
  def funcJOOQMapperName: String = s"as${objName}"

  def stringToSourceValue(s: String)(implicit currentGroupName: Locator): String = {
    val objEnumValue = enumValues.filter(_.key === s).head
    val objValueName = {if (currentGroupName === group) objValuesName else s"${group}.${objValuesName}"} + "." + objEnumValue.name.capitalize

    if (keyIsMandatory) objValueName else s"Some(${objValueName}})"

  }

  def toXML(implicit resolver: SchemaDef): Elem = {
    <values>
      {enumValues.map(ev => {
        ev.keyValue.foldLeft(<value/>)((acc, map) => acc % Attribute(None, map._1, Text(map._2), Null))
      }
      )}
    </values>
  }
}

//---------------------------------------------------------------------------------

