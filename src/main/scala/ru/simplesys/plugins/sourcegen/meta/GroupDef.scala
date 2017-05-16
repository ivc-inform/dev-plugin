package ru.simplesys.plugins
package sourcegen
package meta

import ru.simplesys.meta.types.Locator
import scala.xml.Node

//---------------------------------------------------------------------------------

class GroupDefData(val prefix: String, val name: String, val caption: String)

//---------------------------------------------------------------------------------

trait GroupDef {
    self: GroupDefData =>
    val selfRef: Locator = name
    def name: String
    def prefix: String
    def caption: String
}

//---------------------------------------------------------------------------------

object GroupDef {
    def apply(x: Node): GroupDef = {
        val groupPrefix = (x \ "@prefix").text
        val groupName = (x \ "@name").text
        val groupCaption = (x \ "@caption").text

        new GroupDefData(groupPrefix, groupName, groupCaption) with GroupDef
    }
}