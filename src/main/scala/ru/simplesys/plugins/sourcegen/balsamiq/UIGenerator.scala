package ru.simplesys.plugins
package sourcegen
package balsamiq

import scala.xml.{Elem, XML, Node}
import com.simplesys.common.equality.SimpleEquality._
import ru.simplesys.plugins.sourcegen.meta._
import scalaz.syntax.std.option._
import sbt._

class UIGeneratorData(val schema: SchemaDef, val mockups: Seq[Mockup])

trait UIGenerator {
  self: UIGeneratorData =>
  def mockups: Seq[Mockup]
  def resolveMockup(s: String): Mockup = mockups.find(_.fileName === s).get
  def generateFiles(outUIDir: File)(implicit log: Logger): Seq[File] = {
    log.info("entered generateMockupUI")
    IO.delete(outUIDir)
    outUIDir.mkdirs()
    val controls: Map[String, Option[IUIControl]] = mockups.map(x => (x.fileName, x.toUIControl(CanvasProps(widthPCT = Some(100), heightPCT = Some(100)))(this)))(collection.breakOut)
    controls.flatten {case (fn, optUI) =>
      optUI.map {ui =>
        val newFN = if (fn.toLowerCase.endsWith(".bmml")) {
          val index = fn.lastIndexOf('.')
          fn.substring(0, index) + ".xml"
        } else fn

        val coveredXML = <RootPane xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns="http://simpleSys.ru/xml/library/app"
                xmlns:isc="http://simpleSys.ru/xml/library/ISC"
                xsi:schemaLocation="http://simpleSys.ru/xml/library/app http://toucan.simplesys.lan/xml/xsd/v1.0.0-1/schemaApp.xsd">
            <RootCanvas>
              {ui.toXML}
            </RootCanvas>
          </RootPane>

        XmlUtil.save(coveredXML, outUIDir / newFN)
      }
    }.toSeq
  }
}

object UIGenerator {
  def apply(boSchema: SchemaDef, xmlNodes: Array[(String, Elem)]): UIGenerator = {

    val mockups = {for ((caption, xmlPiece) <- xmlNodes.toSeq) yield {
      (xmlPiece \\ "mockup").map(Mockup(caption, _))
    }}.flatten

    //      res.classesMap.keys.foreach(println _)
    val res = new UIGeneratorData(boSchema, mockups) with UIGenerator
    res
  }

  def apply(boSchema: SchemaDef, files: Seq[File]): UIGenerator = {
    val xmlPieces: Array[(String, Elem)] = files.map {
      x =>
      //println(x.getAbsolutePath)
        (x.getName, XML.load(new java.io.InputStreamReader(new java.io.FileInputStream(x), XmlUtil.Encoding)))
    }(collection.breakOut)
    apply(boSchema, xmlPieces)
  }}
