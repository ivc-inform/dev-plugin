package ru.simplesys.plugins.sourcegen.app

import com.simplesys.common.Strings._
import com.simplesys.scalaGen._

import scala.collection.mutable.ArrayBuffer


object SeqScalaClassJSON {
    def apply(classes: ScalaClassJSON*) = new SeqScalaClassJSON("",classes: _ *)
    def apply(opt:String, classes: ScalaClassJSON*) = new SeqScalaClassJSON(opt,classes: _ *)
    def apply() = new SeqScalaClassJSON("")
}

class SeqScalaClassJSON(opt: String, classes: ScalaClassJSON*) extends ScalaPropertyElement {
    private val _classes = ArrayBuffer.empty[ScalaObjectElement] ++ classes
    protected val className = "Seq"

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
            s"$className(" + res.newLine + spaces(indent) + s")${if (opt != "") s".$opt" else opt}"
        else
            s"$className()${if (opt != "") s".$opt" else opt}"
    }
}

object ArrayScalaClassJSON {
    def apply(classes: ScalaClassJSON*) = new ArrayScalaClassJSON(classes: _ *)
    def apply() = new ArrayScalaClassJSON()
}

class ArrayScalaClassJSON(classes: ScalaClassJSON*) extends SeqScalaClassJSON("", classes: _*) {
    override protected val className = "ArrayDyn"
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


