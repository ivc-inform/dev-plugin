package ru.simplesys.plugins
package sourcegen
package balsamiq

import scala.xml.{Elem, XML, Node}
import com.simplesys.common.equality.SimpleEquality._
import com.simplesys.common.XMLs._
import com.simplesys.common.Strings._
import ru.simplesys.plugins.sourcegen.meta.SealedEnum
import ru.simplesys.coreutil.SealedEnumRuntime
import scalaz.syntax.std.option._

//--UIStates--------------------------------------------------------------------------
sealed trait MockupControlState extends SealedEnum

object MockupControlStateUp extends MockupControlState {
  val key = "up"
}

object MockupControlStateSelected extends MockupControlState {
  val key = "selected"
}

object MockupControlStateFocused extends MockupControlState {
  val key = "focused"
}

object MockupControlStateDisabled extends MockupControlState {
  val key = "disabled"
}

object MockupControlStateDisabledSelected extends MockupControlState {
  val key = "disabledSelected"
}


object MockupControlState {
  private val values = SealedEnumRuntime.values[MockupControlState]
  private val mappedKeys: Map[String, MockupControlState] = values.map(x => (x.toString, x))(collection.breakOut)
  def getObject(objName: String): MockupControlState = mappedKeys(objName)
  def apply(x: String): MockupControlState = mappedKeys(x)
}
//--UIStates--------------------------------------------------------------------------

//--MockupControlProperties-----------------------------------------------------------
class MockupControlProperties(val text: Option[String],
                              val customID: Option[String],
                              val href: Option[String],
                              val hrefs: Option[String],
                              val selectedIndex: Option[Int],
                              val src: Option[String],
                              val state: Option[MockupControlState],
                              val markup: Option[Boolean])

object MockupControlProperties {
  def apply(e: Node): MockupControlProperties = {
    val text = (e \ "text").textOption.map(_.decodePercentEncoded)
    val customID = (e \ "customID").textOption
    val href = (e \ "href").textOption.map(_.decodePercentEncoded)
    val hrefs = (e \ "hrefs").textOption.map(_.decodePercentEncoded)
    val src = (e \ "src").textOption.map(_.decodePercentEncoded)
    val selectedIndex = (e \ "selectedIndex").textOption.map(_.toInt).filter(_ !== -1)
    val state = (e \ "state").textOption.map(MockupControlState(_))
    val markup = (e \ "markup").textOption.map(_.toBoolean)
    new MockupControlProperties(text, customID, href, hrefs, selectedIndex, src, state, markup)
  }
}
//--MockupControlProperties-----------------------------------------------------------

//--BaseMockupControl-----------------------------------------------------------------
class MockupControlData(val controlID: Int,
                        val controlTypeID: String,
                        val x: Int,
                        val y: Int,
                        val w: Int,
                        val h: Int,
                        val measuredW: Int,
                        val measuredH: Int,
                        val properties: Option[MockupControlProperties],
                        val markup: Boolean/*,
                       val invisible: Boolean*/)

trait MockupControl extends RectangleShape with RectangleBySizeBase {
  self: MockupControlData =>
  def controlID: Int
  def controlTypeID: String
  def x: Int
  def y: Int
  def width: Int = if (w === -1) measuredW else w
  def height: Int = if (h === -1) measuredH else h
//  def markup: Boolean - inherited from RectangleShape
  def text: Option[String] = properties.flatMap(_.text)
  def customID: Option[String] = properties.flatMap(_.customID)
  def properties: Option[MockupControlProperties]

  override def toString: String = s"controlID: ${controlID} controlTypeID: ${controlTypeID} x.y: ${x}.${y} width.height: ${width}.${height}${text.map(t => s", text: ${t}") | ""}"
}

object MockupControl {
  def apply(e: Node): MockupControl = {
    val controlID = (e \ "@controlID").text.toInt
    val controlTypeID = (e \ "@controlTypeID").text
    val x = (e \ "@x").text.toInt
    val y = (e \ "@y").text.toInt
    val w = (e \ "@w").text.toInt
    val h = (e \ "@h").text.toInt
    val measuredW = (e \ "@measuredW").text.toInt
    val measuredH = (e \ "@measuredH").text.toInt

    val properties = (e \ "controlProperties").map(MockupControlProperties(_)).headOption
    val markupOption = properties.flatMap(_.markup)

    val textToAnalyze = properties.flatMap(_.text)
    controlTypeID match {
      case "com.balsamiq.mockups::Component" => MockupControl(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markupOption | false)

      case "com.balsamiq.mockups::StickyNote"
        if markupOption.filter(_ === false).isEmpty &&
          textToAnalyze.filter(_.lines.toList.head.trim.toUpperCase.startsWith("DS")).isDefined =>
        new {
          val dataSourceMappingList: Seq[String] = textToAnalyze.map(_.lines.toList.tail.map(_.trim).filter(!_.isEmpty)).get
        } with MockupControlData(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, true) with MockupDataSourceMapControl

      case "com.balsamiq.mockups::StickyNote"
        if markupOption.filter(_ === false).isDefined ||
          textToAnalyze.filter(_.lines.toList.head.trim.toUpperCase.startsWith("DS")).isEmpty =>
        new MockupControlData(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markupOption | true) with StickyNoteControl

      case "com.balsamiq.mockups::CallOut"
        if markupOption.filter(_ === false).isEmpty &&
          textToAnalyze.filter(_.startsWith("DS:")).isDefined => new {
        val dataSourceRef = textToAnalyze.get.substring(3).trim
      } with MockupControlData(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, true) with MockupDataSourceRefControl

      case "com.balsamiq.mockups::CallOut"
        if markupOption.filter(_ === false).isDefined &&
          textToAnalyze.filter(_.startsWith("DS:")).isEmpty =>
        new MockupControlData(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markupOption | true) with CalloutControl

      case "com.balsamiq.mockups::VCurly" => MockupControl(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markupOption | true)

      case "com.balsamiq.mockups::DataGrid" => new MockupControlData(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markupOption | false) with DataGridControl

      case "com.balsamiq.mockups::TabBar" => new MockupControlData(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markupOption | false) with TabBarControl

      case "com.balsamiq.mockups::HSplitter" => new MockupControlData(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markupOption | false) with HSplitterControl
      case "com.balsamiq.mockups::VSplitter" => new MockupControlData(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markupOption | false) with VSplitterControl

      case "com.balsamiq.mockups::Canvas" => new MockupControlData(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markupOption | false) with DynamicFormControl

      case "com.balsamiq.mockups::TextInput" => new MockupControlData(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markupOption | false) with FormItemControl
      case "com.balsamiq.mockups::ComboBox" => new MockupControlData(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markupOption | false) with FormItemControl
      case "com.balsamiq.mockups::DateChooser" => new MockupControlData(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markupOption | false) with FormItemControl
      case "com.balsamiq.mockups::TextArea" => new MockupControlData(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markupOption | false) with FormItemControl

      case _ => MockupControl(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markupOption | false)
    }
  }

  def apply(controlID: Int,
            controlTypeID: String,
            x: Int, y: Int, w: Int, h: Int, measuredW: Int, measuredH: Int,
            properties: Option[MockupControlProperties],
            markup: Boolean): MockupControl = new MockupControlData(controlID, controlTypeID, x, y, w, h, measuredW, measuredH, properties, markup) with MockupControl with NonUILikeControlImpl
}
//--BaseMockupControl-----------------------------------------------------------------

//--FakeControl-----------------------------------------------------------------------
trait FakeProvider {
  def fakeChildren: Seq[RectangleShape]
}

trait FakeControl extends RectangleShape {
  self: FakeProvider with Rectangle with RectangleBase =>

  override def markup = false
  override def childrenInclMarkup(implicit currentMockup: Mockup): Seq[RectangleShape] = fakeChildren
  override def children(implicit currentMockup: Mockup): Seq[RectangleShape] = fakeChildren

  override def dsLink(implicit currentMockup: Mockup): Option[DataSourceLink] = None
}
//--FakeControl-----------------------------------------------------------------------
