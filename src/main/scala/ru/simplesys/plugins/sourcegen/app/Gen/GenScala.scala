package ru.simplesys.plugins.sourcegen.app.Gen

import java.net.URI

import com.simplesys.common.Strings._
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.isc.system.typesDyn.OperatorId
import com.simplesys.scalaGen.{ScalaClassJSONProperties, ScalaClassJSONProperty, ScalaClassJSONPropertyClassJSON, ScalaClassesJSON, _}
import ru.simplesys.plugins.sourcegen.app.xml.IscElem
import ru.simplesys.plugins.sourcegen.app.{ArrayScalaClassJSON, JsonListScalaClassJSON, SeqScalaClassJSON}
import sbt.{File, Logger}

import scalax.file.Path

abstract class GenScala {
    val appFilePath: Path
    val schemaPath: URI
    val outFilePath: Path
    val packageName: String
    val logger: Logger
    def create: File

    protected def makeScalaCode(root: IscElem, includingGroupName: Seq[String] = Seq.empty): ScalaBody = {
        root.getScalaClassJSONProperty match {
            case ScalaClassJSONProperty(_, value) => value match {
                case ScalaClassJSONPropertyClassJSON(value) =>
                    ScalaBody(value addProperties makeCode(root, includingGroupName))
                case propertyElement =>
                    throw new RuntimeException(s"Unknown implemantation for type : ${propertyElement.toString.dblQuoted}")
            }
            case propertyElement =>
                throw new RuntimeException(s"Unknown implemantation for type : ${propertyElement.toString.dblQuoted}")
        }
    }

    protected def makeScalaCodeJS(root: IscElem, includingGroupName: Seq[String] = Seq.empty): ScalaBody = {
        root.getScalaClassJSONPropertyJS match {
            case ScalaClassJSONProperty(_, value) => value match {
                case ScalaClassJSONPropertyClassJSON(value) =>
                    ScalaBody(value addProperties makeCodeJS(root, includingGroupName))
                case propertyElement =>
                    throw new RuntimeException(s"Unknown implemantation for type : ${propertyElement.toString.dblQuoted}")
            }
            case propertyElement =>
                throw new RuntimeException(s"Unknown implemantation for type : ${propertyElement.toString.dblQuoted}")
        }
    }

    protected def makeCode(elements: IscElem, includingGroupName: Seq[String] = Seq.empty[String]): ScalaClassJSONProperties = {
        var res = ScalaClassJSONProperties()
        //elements.log

        val group = elements getElements "Group"
        //group.log

        val a = includingGroupName.isEmpty
        val b = group.text === strEmpty
        val c = includingGroupName.exists(_ == group.text)

        if (a || b || c)
            for (element <- elements.child.filter(_.label !== "#PCDATA")) {

                val _element: IscElem = element
                //_element.log

                val propertyElement: ScalaClassJSONProperty = _element.getScalaClassJSONProperty

                propertyElement match {
                    case null =>
                    case ScalaClassJSONProperty(label, scalaPropertElement) =>
                        scalaPropertElement match {
                            case array: ArrayScalaClassJSON =>
                                val _arr = makeCode(_element, includingGroupName)

                                if (!_arr.isEmpty)
                                    res += (label -> (array ++= _arr)).property

                            case array: JsonListScalaClassJSON =>
                                val _arr = makeCode(_element, includingGroupName)

                                if (!_arr.isEmpty)
                                    res += (label -> (array ++= _arr)).property

                            case ScalaClassJSONPropertyClassJSON(value) =>
                                val _arr = makeCode(_element, includingGroupName)

                                if (!_arr.isEmpty) {
                                    value addProperties _arr
                                    res += (label -> value).property
                                }
                            case value: ScalaPropertyElement =>
                                res += propertyElement
                            case _ =>
                                throw new RuntimeException(s"Unknown implemantation label:${label} type : ${scalaPropertElement.toString.dblQuoted}")
                        }
                }
            }
        res
    }

    protected def makeCodeJS(elements: IscElem, includingGroupName: Seq[String] = Seq.empty[String]): ScalaClassJSONProperties = {
        var res = ScalaClassJSONProperties()
        //elements.log

        val group = elements getElements "Group"
        //group.log

        val a = includingGroupName.isEmpty
        val b = group.text === strEmpty
        val c = includingGroupName.exists(_ == group.text)

        if (a || b || c)
            for (element <- elements.child.filter(_.label !== "#PCDATA")) {

                val _element: IscElem = element
                //_element.log

                val propertyElement: ScalaClassJSONProperty = _element.getScalaClassJSONPropertyJS

                propertyElement match {
                    case null =>
                    case ScalaClassJSONProperty(label, scalaPropertElement) =>
                        scalaPropertElement match {
                            case ScalaClassJSONPropertyClassJSON(value) =>
                                val _arr = makeCodeJS(_element, includingGroupName)

                                if (!_arr.isEmpty) {
                                    value addProperties _arr
                                    res += (label.unCapitalize -> value).property
                                }

                            case array: SeqScalaClassJSON =>
                                val _arr = makeCodeJS(_element, includingGroupName)

                                if (!_arr.isEmpty)
                                    res += (label.unCapitalize -> (array ++= _arr)).property

                            case value: ScalaPropertyElement =>
                                res += propertyElement
                            case _ =>
                                throw new RuntimeException(s"Unknown implemantation label:${label} type : ${scalaPropertElement.toString.dblQuoted}")
                        }
                }
            }
        res
    }
}

abstract class GenScala1 extends GenScala {
    def createSeq: Seq[File]
}
