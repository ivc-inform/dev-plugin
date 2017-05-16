package ru.simplesys.plugins
package sourcegen
package meta

import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.common.Strings._

trait EnumProviderMetaGen {
    self: EnumProvider =>

    def genEnumObjectsForClass(implicit resolver: SchemaDef): String = {
        val out = new StringBuilder()
        //       println(objName)
        val objDefs = enumValues.map(value => {
            val attrs = value.keyValue.withFilter {
                case (keyV, valueV) => keyV !== keyMemberName
            }.map {
                case (keyV, valueV) => {

                    val curr = members(keyV)
                    val dataType = curr._1
                    val isMandatory = curr._2
                    val valueString = dataType.simpleScalaType.stringToSourceValueConditional(isMandatory, valueV)
                    s"    val ${keyV} = ${valueString}"
                }
            }

            val optionalAttrsWOValues = members filter {
                case (attrName, (attrType, isMandatory)) => !value.keyValue.keySet.contains(attrName) && !isMandatory
            } map {
                case (attrName, (attrType, isMandatory)) => s"    val ${attrName} = None"
            }

            s"""|  case object ${value.name.capitalize} extends ${objName} {
          |    val key = ${keyMemberType.simpleScalaType.stringToSourceValueConditional(keyIsMandatory, value.keyValue(keyMemberName))}
          |${attrs.mkString("\n")}
          |${optionalAttrsWOValues.mkString("\n")}
          |  }
          |""".stripMargin
        })

        val forUnzipping = members filterNot {
            case (name, (dataType, isMandatory)) => name == keyMemberName
        } map {
            case (name, (dataType, isMandatory)) => (s"${name}: ${dataType.simpleScalaType.toStringConditional(isMandatory)}", dataType.simpleScalaType.toStringConditional(isMandatory), name)
        }
        val paramList = forUnzipping.map(_._1)
        val returnTypes = forUnzipping.map(_._2)
        val names = forUnzipping.map(_._3)
        val defs = members map {
            case (name, (dataType, isMandatory)) => s"  def ${name}: ${dataType.scalaTypeAsStringConditional(isMandatory)(group, resolver)}${if (name === keyMemberName) " = this" else ""}"
        }

        def keyType = keyMemberType.simpleScalaType.toStringConditional(keyIsMandatory)


//          |object ${objName} extends BaseConverterSealedTrait[${keyType}, ${objName}] {
        val objDef =
            s"""|
          |case object Domain${objName} extends MetaType[${objName}]
          |
          |object ${objName} {
          |  val values = SealedEnumRuntime.values[${objName}]
          |  val default = values.map(x => x)(collection.breakOut)(0)
          |
          |  val mappedKeys: Map[${keyType}, ${objName}] = values.map(x => (x.key, x))(collection.breakOut)
          |  val mappedKeys1: Map[${objName}, ${keyType}] = values.map(x => (x, x.key))(collection.breakOut)
          |
          |  def apply(${keyMemberName}: ${objName}, ${paramList.mkString(", ")}): ${objName} = ${keyMemberName}
          |  def unapply(x: ${objName}): Option[(${objName}, ${returnTypes.mkString(", ")})] = Some((x, ${names.map("x." + _).mkString(", ")}))
          |
          |  implicit def to${objName}(ss: ${keyType}): ${objName} = mappedKeys(ss)
          |  implicit def from${objName}(ss: ${objName}): ${keyType} = mappedKeys1(ss)
          |  implicit def from${objName}2OptionJsonElement(ss: ${objName}): Option[Json${keyType}] = Some(Json${keyType}(from${objName}(ss)))
          |  implicit def from${objName}Opt2OptionJsonElement(ss: Option[${objName}]): Option[Json${keyType}] = ss match {
          |     case None => None
          |     case Some(value) => Some(Json${keyType}(from${objName}(value)))
          |  }
          |  implicit def fromT${objName}Array2OptionJsonElement(ss: Array[${objName}]): Option[Json${keyType}] = if (ss.length == 0) None else Some(Json${keyType}((mappedKeys1(ss(0)))))
          |
          |  implicit def toOpt${objName}(ss: Option[${keyType}]): Option[${objName}] = ss match {
          |      case None => None
          |      case Some(string) => Some(to${objName}(${keyType.unCapitalize}))
          |  }
          |
          |  implicit def fromArray${objName}2${keyType}Array(ss: Array[${objName}]): Array[${keyType}] = if (ss.length == 0) NotValue else toArray(mappedKeys1(ss(0)))
          |  implicit def fromOption${objName}2${keyType}Array(ss: Option[${objName}]): Array[${keyType}] =
          |      ss match {
          |          case None => NotValue
          |          case Some(ss) => toArray(ss)
          |      }
          |}
          |
          |sealed abstract class ${objName} extends SealedCaseClassEnum[${keyType}] {
          |${defs.mkString("\n")}
          |}
          |
          |object ${objValuesName} {
          |${objDefs.mkString("\n")}
          |}
          |
          |""".stripMargin

        out append fill("from EnumProviderMetaGen").newLine
        out append objDef
        out append fill("end from EnumProviderMetaGen")

        out.toString().chmp

    }
}

trait EnumClassProviderMetaGen {
    self: AbstractClassDefMetaGen with EnumProviderMetaGen =>

    // all you need you have in EnumProviderMetaGen
    override def genClassDefs(implicit resolver: SchemaDef): String = ""
}
