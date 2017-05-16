package ru.simplesys.plugins
package sourcegen

import com.simplesys.xml.XML
import sbt._
import java.nio.charset.Charset

object XmlUtil {
    val Encoding = "UTF-8"
    val charset = Charset.forName(Encoding)

    def save(node: xml.Node, file: File) = {
        file.getParentFile.mkdirs()
        XML.save(file.getAbsolutePath, node, Encoding)
        file
    }

    def addChild(n: xml.Node, newChild: xml.Node) = n match {
        case xml.Elem(prefix, label, attribs, scope, child@_*) =>
            xml.Elem(prefix, label, attribs, scope, true, child ++ newChild: _*)
        case _ => sys.error("Can only add children to elements!")
    }
}
