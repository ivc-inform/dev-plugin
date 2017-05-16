package ru.simplesys.plugins.sourcegen.app

import com.simplesys.common.Strings._
import scala.collection.mutable.ArrayBuffer
import com.simplesys.scalaGen._
import com.simplesys.json._

object ArrayScalaClassJSON {
    def apply(classes: ScalaClassJSON*) = new ArrayScalaClassJSON(classes: _ *)
    def apply() = new ArrayScalaClassJSON()
}

class ArrayScalaClassJSON(classes: ScalaClassJSON*) extends ScalaPropertyElement {
    private val _classes = ArrayBuffer(classes: _*)

    def +=(`class`: ScalaClassJSON) = {
        _classes += `class`
        this
    }

    def ++=(`class`: ScalaClassJSON*) = {
        _classes ++= `class`
        this
    }

    def ++=(`class`: ScalaClassesJSON) = {
        _classes ++= `class`.getItems
        this
    }

    def serrialize(indent: Int = 0) = {
        val res = (_classes map (newLine + spaces(indent + com.simplesys.scalaGen.indentSize) + _.serrialize(indent + com.simplesys.scalaGen.indentSize).trim)).mkString(",")
        if (!res.isEmpty)
            "ArrayDyn(" + res.newLine + spaces(indent) + ")"
        else
            "ArrayDyn()"
    }
}

object JsonListScalaClassJSON {
    def apply(classes: ScalaObjectElement*) = new JsonListScalaClassJSON(classes: _ *)
    def apply() = new JsonListScalaClassJSON()
}

class JsonListScalaClassJSON(classes: ScalaObjectElement*) extends ScalaPropertyElement {
    private val _classes = ArrayBuffer(classes: _*)

    def +=(`class`: ScalaObjectElement) = {
        _classes += `class`
        this
    }

    def ++=(`class`: ScalaObjectElement*) = {
        _classes ++= `class`
        this
    }

    def ++=(`class`: ScalaObjectsJSON) = {
        _classes ++= `class`.getItems
        this
    }

    def serrialize(indent: Int = 0) = {
        val res = (_classes map (newLine + spaces(indent + com.simplesys.scalaGen.indentSize) + _.serrialize(indent + com.simplesys.scalaGen.indentSize).trim)).mkString(",")
        if (!res.isEmpty)
            "JsonList(" + res.newLine + spaces(indent) + ")"
        else
            "JsonList()"
    }
}


