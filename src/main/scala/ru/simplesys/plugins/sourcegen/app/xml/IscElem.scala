package ru.simplesys.plugins.sourcegen.app.xml

import com.simplesys.common.Strings._
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.log.Logging
import com.simplesys.scalaGen._
import com.simplesys.xml.Elem
import ru.simplesys.plugins.sourcegen.app._

import scala.xml.{Node, NodeSeq}

object IscElem {
    implicit def apply(elem: Elem): IscElem = new IscElem(elem)
    implicit def apply(elem: Node): IscElem = new IscElem(elem)
    implicit def apply(elem: NodeSeq): IscElem = new IscElem(elem)
    implicit def apply(elem: IscElem): Elem = elem.proxy
}

class IscElem(protected val proxy: Elem) extends Logging {
    def value: String = proxy text

    def label = proxy label

    def `type`: String = proxy.xmlType.toString

    def getStringValue(path: String): String = {
        ((proxy \ path): Elem).selfText
    }

    def getBooleanValue(path: String): Boolean = {
        val res = ((proxy \ path): Elem).selfText
        if (res.isEmpty) false else res.toBoolean
    }

    def isEmpty (path: String): Boolean =  (proxy \ path).isEmpty
    def isDefigned (path: String): Boolean =  !isEmpty(path)

    def getArttributeValue(path: String): String = {
        (proxy \ s"@${path}").text
    }

    def text = proxy.text

    def getElements(path: String): IscElem = proxy \ path

    def getScalaClassJSONProperty: ScalaClassJSONProperty = {
        var res: ScalaClassJSONProperty = null

        label match {
            case "useSelfName" | "JObjectFieldName" | "Calculated" | "GetterType" | "JdbcType" | "Discriminator" | "GenBySeq" =>
            case _ =>
                `type` match {
                    case "DblQuoted" =>
                        res = (label -> value).dblQuotedProperty
                    case "UnQuoted" =>
                        res = (label -> value).UnQuotedProperty
                    case "url" =>
                        res = (label -> value).URLProperty
                    case "urlWithVar" =>
                        val _value = if (value.indexOf("@") == -1) value else value.substring(0, value.indexOf("@"))
                        val _variable = if (value.indexOf("@") == -1) strEmpty else value.substring(value.indexOf("@") + 1)
                        if (_variable !== strEmpty)
                            res = (label -> _value).URLwithVarProperty(_variable.dblQuoted)
                        else
                            res = (label -> _value).URLProperty
                    case "unq" =>
                        res = (label -> value).UnqProperty
                    case "JSFileURL" =>
                        res = (label -> value).URLProperty
                    case "JSFunction" =>
                        res = (label -> value).JSCodeProperty
                    case "integer" =>
                        res = (label -> value.toInt).property
                    case "double" =>
                        res = (label -> value.toDouble).property
                    case "boolean" =>
                        res = (label -> value.toBoolean).property
                    case "string" =>
                        res = (label -> value).property
                    case "IDREF" | "NMTOKEN" =>
                        res = (label -> value.unQuotedValue).property
                    case "IDREF1" | "IDREF2" =>
                        res = (label -> value).UnqProperty
                    case "ID" | "token" | "PersentNumberType" =>
                        res = (label -> value).property
                    case "HTMLString" =>
                        res = (label -> value.ApproxValue).property
                    case "EllipsisString" =>
                        if ((proxy \ ("@Ellipsis")).text == "true")
                            res = (label -> value).EllipsisProperty
                        else
                            res = (label -> value).property
                    case "Any" =>
                        val chld = (proxy: IscElem).child.filter(_.label !== "#PCDATA") head
                        val variant = chld.label

                        if (variant.isEmpty)
                            throw new RuntimeException(s"Unknown implemantation label:${label} type: ${`type`}")


                        val _value = ((proxy \ variant): IscElem).value

                        variant match {
                            case "string" =>
                                res = (label -> _value).property
                            case "clob" =>
                                res = (label -> _value).property
                            case "blob" =>
                                res = (label -> _value).property
                            case "double" =>
                                res = (label -> _value.toDouble).property
                            case "integer" =>
                                res = (label -> _value.toInt).property
                            case "long" =>
                                res = (label -> _value.toDouble).property
                            case "boolean" =>
                                res = (label -> _value.toBoolean).property
                            case "date" =>
                                res = (label -> _value.toLocalDateTime()).property
                            case "localDateTime" =>
                                res = (label -> _value.toLocalDateTime()).property
                            case "dateTime" =>
                                res = (label -> _value.toLocalDateTime()).propertyWithTm
                            case _ =>
                                throw new RuntimeException(s"Unknown implemantation :${label.dblQuoted} type: ${`type`}")
                        }

                        res
                    case _type =>
                        if (_type.indexOf("JsonList") != -1)
                            res = (label -> JsonListScalaClassJSON()).property
                        else if (_type.indexOf("ArrayType") != -1)
                            res = (label -> ArrayScalaClassJSON()).property
                        else if (_type.indexOf("Type") != -1) {
                            val className = _type.substring(0, _type.lastIndexOf("Type")) + _type.substring(_type.lastIndexOf("Type") + 4)

                            val `class` = new ScalaClassJSON {
                                scalaClassGen = className.cls
                            }

                            val useSelfName = getStringValue("useSelfName")
                            if (useSelfName != "")
                                `class`.addParameters(("useSelfName", useSelfName).param)

                            res = (label -> ScalaClassJSONPropertyClassJSON(`class`)).property
                            //res.log
                        } else if (_type.indexOf("Enum1") != -1) {
                            res = (label -> value.unQuotedValue1).property
                        } else if (_type.indexOf("Enum") != -1) {
                            res = (label -> value.unQuotedValue).property
                        } else
                            throw new RuntimeException(s"Unknown implemantation label:${label} type: ${_type}")
                }
        }

        res
    }

    def getScalaClassJSONPropertyJS: ScalaClassJSONProperty = {
        var res: ScalaClassJSONProperty = null

        label match {
            case "useSelfName" | "JObjectFieldName" | "Calculated" | "GetterType" | "JdbcType" | "Discriminator" | "GenBySeq" =>
            case _ =>
                `type` match {
                    case "DblQuoted" =>
                        res = (label.unCapitalize -> value).dblQuotedProperty
                    case "UnQuoted" =>
                        res = (label.unCapitalize -> value).UnQuotedProperty
                    case "url" =>
                        res = (label.unCapitalize -> value).URLProperty
                    case "urlWithVar" =>
                        val _value = if (value.indexOf("@") == -1) value else value.substring(0, value.indexOf("@"))
                        res = (label.unCapitalize -> _value).property
                    case "unq" =>
                        res = (label.unCapitalize -> value).UnqProperty
                    case "JSFileURL" =>
                        res = (label.unCapitalize -> value).URLProperty
                    case "JSFunction" =>
                        res = (label.unCapitalize -> value).JSCodeProperty
                    case "integer" =>
                        res = (label.unCapitalize -> value.toInt).property
                    case "double" =>
                        res = (label.unCapitalize -> value.toDouble).property
                    case "boolean" =>
                        res = (label.unCapitalize -> value.toBoolean).property
                    case "string" =>
                        res = (label.unCapitalize -> value).property
                    case "IDREF" | "NMTOKEN" =>
                        res = (label.unCapitalize -> value.unQuotedValue).property
                    case "IDREF1" | "IDREF2" =>
                        res = (label.unCapitalize -> value).UnqProperty
                    case "token" if label == "ID" =>
                        res = (label -> value).property
                    case "token" if label == "Type" =>
                        res = ("`type`" -> s"FieldType.$value.opt".unQuotedValue).property
                    case "token" =>
                        res = (label.unCapitalize -> value).property
                    case "ID" | "PersentNumberType" =>
                        res = (label.unCapitalize -> value).property
                    case "HTMLString" =>
                        res = (label.unCapitalize -> value.ApproxValue).property
                    case "EllipsisString" =>
                        if ((proxy \ ("@Ellipsis")).text == "true")
                            res = (label.unCapitalize -> value).EllipsisProperty
                        else
                            res = (label.unCapitalize -> value).property
                    case "Any" =>
                        val chld = (proxy: IscElem).child.filter(_.label !== "#PCDATA") head
                        val variant = chld.label

                        if (variant.isEmpty)
                            throw new RuntimeException(s"Unknown implemantation label:${label} type: ${`type`}")


                        val _value = ((proxy \ variant): IscElem).value

                        variant match {
                            case "string" =>
                                res = (label.unCapitalize -> _value).property
                            case "clob" =>
                                res = (label.unCapitalize -> _value).property
                            case "blob" =>
                                res = (label.unCapitalize -> _value).property
                            case "double" =>
                                res = (label.unCapitalize -> _value.toDouble).property
                            case "integer" =>
                                res = (label.unCapitalize -> _value.toInt).property
                            case "long" =>
                                res = (label.unCapitalize -> _value.toDouble).property
                            case "boolean" =>
                                res = (label.unCapitalize -> _value.toBoolean).property
                            case "date" =>
                                res = (label.unCapitalize -> _value.toLocalDateTime()).property
                            case "localDateTime" =>
                                res = (label.unCapitalize -> _value.toLocalDateTime()).property
                            case "dateTime" =>
                                res = (label.unCapitalize -> _value.toLocalDateTime()).propertyWithTm
                            case _ =>
                                throw new RuntimeException(s"Unknown implemantation :${label.dblQuoted} type: ${`type`}")
                        }

                        res
                    case _type =>
                        if (_type.indexOf("JsonList") != -1)
                            res = (label.unCapitalize -> SeqScalaClassJSON("opt")).property
                        else if (_type.indexOf("ArrayType") != -1)
                            res = (label.unCapitalize -> SeqScalaClassJSON("opt")).property
                        else if (_type.indexOf("Type") != -1) {
                            val className = _type.substring(0, _type.lastIndexOf("Type")) + _type.substring(_type.lastIndexOf("Type") + 4)

                            val `class` = new ScalaClassJSON {
                                scalaClassGen = className.replace("Dyn", "Props").cls
                                wrappadOperator = if (className.contains("SimpleType")) "SimpleType.create" else ""
                            }

                            res = (label.unCapitalize -> ScalaClassJSONPropertyClassJSON(`class`)).property
                            //res.log
                        } else if (_type.indexOf("Enum1") != -1) {
                            res = (label.unCapitalize -> value.unQuotedValue1).property
                        } else if (_type.indexOf("Enum") != -1) {
                            val _value = value match {
                                case "dtftJSON" => "DSDataFormat.json.opt"
                                case "dsprtPostXML" => "DSProtocol.postXML.opt"
                                case "dsOptTypeAdd" => "DSOperationType.add.opt"
                                case "dsOptTypeFetch" => "DSOperationType.fetch.opt"
                                case "dsOptTypeRemove" => "DSOperationType.remove.opt"
                                case "dsOptTypeUpdate" => "DSOperationType.update.opt"
                            }
                            res = (label.unCapitalize -> _value.unQuotedValue).property
                        } else
                            throw new RuntimeException(s"Unknown implemantation label:${label} type: ${_type}")
                }
        }

        res
    }
}
